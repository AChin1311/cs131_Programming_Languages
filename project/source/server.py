import asyncio
import logging
from logging import handlers
import sys
import os
import time
import datetime
import re
import ssl
import json
import aiohttp

SERVER_PORT = {'Goloman': 11981, 'Hands': 11982, 'Holiday': 11983, 'Welsh': 11984, 'Wilkes': 11985}
SERVER_NEIGHBOR = {'Goloman':['Hands', 'Holiday', 'Wilkes'], 
                    'Hands':['Wilkes', 'Goloman'], 
                    'Holiday':['Welsh', 'Wilkes', 'Goloman'], 
                    'Welsh':['Holiday'], 
                    'Wilkes':['Goloman', 'Hands', 'Holiday']}

def handle_exception(loop, context):
  try:
    exception = context['exception']
    if isinstance(exception, ConnectionRefusedError):
        logging.warning('Cannot connect neighboring server.')
    else:
        logging.warning('Error {}'.format(context['exception']))
  except KeyError:
    logging.warning('Unknown Error {}'.format(context['message']))

client_status = {}

class ServerProtocol(asyncio.Protocol):
  def __init__(self, server_name):
    self.server_name = server_name
    self.neighbors = SERVER_NEIGHBOR[server_name]

  def connection_made(self, transport):
    peername = transport.get_extra_info('peername')
    print('Connection from {}'.format(peername))
    logging.info('Connection from {}'.format(peername))
    self.transport = transport

  def data_received(self, data):
    message = data.decode()
    print('Data received: {!r}'.format(message))
    logging.info('Data received: {!r}'.format(message))

    query = message.split()
    query_t = query[0]
    if query_t == 'IAMAT' and self.check_IAMAT(query):
      # IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
      response = self.handle_IAMAT(query[1], query[2], query[3])   
    elif query_t == 'AT' and self.check_AT(query):
      # AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
      response = self.handle_AT(query[-1], query[3], query)
    elif query_t == 'WHATSAT' and self.check_WHATSAT(query):
      # WHATSAT kiwi.cs.ucla.edu 10 5
      client_id = query[1]
      if client_id not in client_status:
        logging.error("client id {} is not specified.".format(client_id))
        return
      self.handle_WHATSAT(query[1], query[2], query[3])
      return
    else:
      response = '? ' + message + '\n'
    
    if response != "":
      print('Send: {!r}'.format(response))
      logging.info('Send: {!r}'.format(response))
      self.transport.write(response.encode())
      print('Sent output response: {!r}'.format(response))
      logging.info('Sent output response: {!r}'.format(response))
    elif response[0] == '?':
      logging.info('query is invalid: {!r}'.format(message))
      self.transport.write(response.encode())
      print('Sent output response: {!r}'.format(response))
      logging.info('Sent output response: {!r}'.format(response))

    self.transport.close()
    print('Close the client socket')
    logging.info('Close the client socket')
    
    return

  def update_client(self, client_id, status):
    update = False        
    if status.split()[3] != client_id:
      logging.error('Cannot update client status')
      return update
    if client_id in client_status:
      client_prev_time = float(client_status[client_id].split()[5])
      client_cur_time = float(status.split()[5])
      if client_cur_time > client_prev_time:
        client_status[client_id] = status
        update = True
    else:
      client_status[client_id] = status
      update = True
    
    if update:
      logging.info('Client {} is updated.'.format(client_id))
    else:
      logging.warning('Clinet {} is NOT updated.'.format(client_id))

    return update
  def propagate(self, server_name, message):
    coro = loop.create_connection(lambda: ServerClientProtocol(message, server_name), '127.0.0.1', SERVER_PORT[server_name])
    loop.create_task(coro)

  def flood(self, message):
    # logging.info('message {}.'.foramt(message))
    visited_servers = message.split()[6:]
    logging.info('Already visited {}.'.format(visited_servers))
    for server_name in self.neighbors:
      if server_name not in visited_servers:
        logging.info('Trying to connect {}.'.format(server_name))
        self.propagate(server_name, message)

  def first_flood(self, message):
    for server_name in self.neighbors:
      server_msg = message
      for n in self.neighbors:
        if n != server_name:
          server_msg += ' ' + n
      server_msg += ' ' + self.server_name
      logging.info('Trying to connect {}.'.format(server_name))
      self.propagate(server_name, server_msg)

  def check_location(self, location):
    try:
      loc_list = location.replace('-', ' -').replace('+', ' +').split()
      lat, lng = float(loc_list[0]), float(loc_list[1])
    except:
      print("location error")
      return False
    if lat < -90 or lat > 90 or lng < -180 or lng > 180:
      print("location error2")
      return False
    return True

  def check_time(self, m_time):
    try:
      time = float(m_time)
      datetime.datetime.utcfromtimestamp(time)
    except ValueError:
      print("time error")
      return False
    return True

  def check_IAMAT(self, query):
    if len(query) != 4:
      logging.error('IAMAT takes 3 arguments.')
      return False
    location = query[2]
    m_time = query[3]
    return self.check_location(location) and self.check_time(m_time)
  
  def check_AT(self, query):
    if len(query) < 6:
      logging.error('AT takes at least 6 arguments.')
      return False
    server = query[1]
    time_delta = query[2]
    location = query[4]
    m_time = query[5]

    try:
      float(time_delta)
    except:
      return False
    return (server in SERVER_PORT) and self.check_location(location) and self.check_time(m_time)

  def check_WHATSAT(self, query):
    if len(query) != 4:
      logging.error('WHATSAT takes 4 arguments.')
      return False
    # WHATSAT kiwi.cs.ucla.edu 10 5
    radius = query[2]
    bound = query[3]
    try:
      float(radius)
      int(bound)
    except:
      return False
    return True

  def handle_AT(self, origin, client_id, message):
    # AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997 Goloman
    peername = self.transport.get_extra_info('peername')
    logging.info('Connection {} is propagation from server {}'.format(peername, origin))

    status = ' '.join(message[:6])
    self.update_client(client_id, status)

    flood_msg = ' '.join(message+[self.server_name])
    self.flood(flood_msg)
    return 'Sever {} received updated location from client {}.'.format(self.server_name, client_id)

  def handle_IAMAT(self, ID, location, m_time):
    # receive: IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
    # response: AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
    time_diff = time.time() - float(m_time)
    time_diff_str = '{:.9f}'.format(time_diff)
    if time_diff > 0:
      time_diff_str = '+' + time_diff_str
    
    response = ' '.join(['AT', self.server_name, time_diff_str, ID, location, m_time])
    if self.update_client(ID, response):
      self.first_flood(response)
    else:
      logging.info('Location of client {} is NOT propagated.'.format(ID))
    print(ID, "client_status: ", client_status[ID])
    return client_status[ID]
  
  def handle_WHATSAT(self, client_id, radius_str, info_bound_str):
    # WHATSAT kiwi.cs.ucla.edu 10 5
    
    loc_str = client_status[client_id].split()[4]
    lat, lng = loc_str.replace('+', ' +').replace('-', ' -').split()
    
    radius = str(float(radius_str)*1000)
    info_bound = int(info_bound_str)
    
    # host = 'maps.googleapis.com'
    API_KEY = 'AIzaSyBTm1FoHs4WAxUzHtiQQHvl5UnwckZnk5E'
    url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key='
    url += API_KEY + '&location=' + lat + ',' + lng + '&radius=' + radius
    
    loop = asyncio.get_event_loop()
    loop.create_task(self.do_request(url, info_bound, client_status[client_id], self.transport))

  async def do_request(self, url, bound, header, transport):
    async with aiohttp.ClientSession() as session:
      async with session.get(url) as resp:
        data = await resp.text()
        self.handle_json(data, bound, header, transport)

  def handle_json(self, data, bound, header, transport):
    json_obj = json.loads(data)
    if len(json_obj['results']) > bound:
      json_obj['results'] = json_obj['results'][:bound]
    res = header + '\n' + json.dumps(json_obj, indent=2) + '\n\n'
      
    transport.write(res.encode())
    logging.info('Sent output response: {!r}'.format(res))
    transport.close()
    peername = transport.get_extra_info('peername')
    logging.info('Dropped connection from {}'.format(peername))


class ServerClientProtocol(asyncio.Protocol):
  def __init__(self, message, server_name):
    self.message = message
    self.server_name = server_name

  def connection_made(self, transport):
    logging.info('Propagated location data to server {}'.format(self.server_name))
    logging.info('Connected to server {}'.format(self.server_name))
    self.transport = transport
    transport.write(self.message.encode())
    
  def connection_lost(self, exc):
    self.transport.close()
    logging.info('Dropped connection to server {}'.format(self.server_name))

if __name__ == '__main__':
  if len(sys.argv) != 2:
    print("need exactly 2 arguments")
    sys.exit(1)

  server_name = sys.argv[1]
  assert(server_name in SERVER_NEIGHBOR)

  logfilename = server_name+'.log'
  handler = handlers.RotatingFileHandler(logfilename, mode='w', backupCount=1)
  if os.path.isfile(logfilename):
    handler.doRollover()

  logging.basicConfig(filename= logfilename, level=logging.INFO)
  server_port = SERVER_PORT[server_name]

  loop = asyncio.get_event_loop()
  loop.set_exception_handler(handle_exception)

  # Each client connection will create a new protocol instance
  coro = loop.create_server(lambda: ServerProtocol(server_name), '127.0.0.1', server_port)
  server = loop.run_until_complete(coro)

  # Serve requests until Ctrl+C is pressed
  print('Serving on {}'.format(server.sockets[0].getsockname()))
  logging.info('Serving on {}'.format(server.sockets[0].getsockname()))
  try:
      loop.run_forever()
  except KeyboardInterrupt:
      pass

  # Close the server
  server.close()
  loop.run_until_complete(server.wait_closed())
  loop.close()
