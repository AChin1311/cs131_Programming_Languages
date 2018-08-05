import asyncio
SERVER_PORT = {'Goloman': 11981, 'Hands': 11982, 'Holiday': 11983, 'Welsh': 11984, 'Wilkes': 11985} 

class EchoClientProtocol(asyncio.Protocol):
    def __init__(self, message, loop):
        self.message = message
        self.loop = loop

    def connection_made(self, transport):
        transport.write(self.message.encode())
        print('Data sent: {!r}'.format(self.message))

    def data_received(self, data):
        print('Data received: {!r}'.format(data.decode()))

    def connection_lost(self, exc):
        print('The server closed the connection')
        print('Stop the event loop')
        self.loop.stop()

if __name__ == '__main__':
  server_name = 'Hands'
  message1 = 'IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997'
  port_num = SERVER_PORT[server_name]

  loop = asyncio.get_event_loop()
  coro = loop.create_connection(lambda: EchoClientProtocol(message1, loop),
                                '127.0.0.1', port_num)
  loop.run_until_complete(coro)
  loop.run_forever()
  

  loop = asyncio.get_event_loop()
  message2 = 'WHATSAT kiwi.cs.ucla.edu 10 5'
  coro = loop.create_connection(lambda: EchoClientProtocol(message2, loop),
                                '127.0.0.1', port_num)
  loop.run_until_complete(coro)
  loop.run_forever()
  loop.close()
  