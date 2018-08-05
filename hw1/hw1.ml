let rec find e l = match l with
  | [] -> false
  | x::xs ->
    if e = x then true
    else find e xs;;

let rec subset a b = match a with
  | [] -> true
  | x::xs -> 
    if find x b then subset xs b
    else false;;

let equal_sets a b =
  subset a b && subset b a;;

let rec set_union a b = match a with
  | [] -> b
  | x::xs ->
    if find x b then set_union xs b
    else b@[x];;

let rec set_intersection a b = match a with
  | [] -> []
  | x::xs ->
    if find x b then x::(set_intersection xs b)
    else set_intersection xs b;;

let rec set_diff a b = match a with
  | [] -> []
  | x::xs ->
    if find x b then set_diff xs b
    else x::(set_diff xs b);;

let rec computed_fixed_point eq f x =
  if (eq (f x) x) then x
  else computed_fixed_point eq f (f x);;

let rec compute_p f p x = 
  if p = 0 then x
  else compute_p f (p-1) (f x);;

let rec computed_periodic_point eq f p x =
  if (eq (compute_p f p x) x) then x
  else computed_periodic_point eq f p (f x);;

let rec while_away s p x =
  if p x then x::(while_away s p (s x))
  else [];;

let rec unfold a b =
  if a = 0 then []
  else b::(unfold (a-1) b);;

let rec rle_decode lp = match lp with
  | [] -> []
  | (a, b)::xs -> (unfold a b)@(rle_decode xs);;



type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let compare_sets a b = equal_sets (snd a) (snd b);;

let is_terminal x terminal_list = match x with
  | T _ -> true
  | N s -> if find s terminal_list then true else false;;


let rec remove_terminal rhs terminal_list = match rhs with
  | [] -> true
  | x::xs -> if is_terminal x terminal_list then remove_terminal xs terminal_list
    else false;;

let rec wrap rules terminal_list = match rules with
  | [] -> terminal_list
  | (lhs, rhs)::xs ->  
    if remove_terminal rhs terminal_list then wrap xs (lhs::terminal_list)
    else wrap xs terminal_list;;

let wrapper (rules, terminal_list) = 
  rules, (wrap rules terminal_list);;

let get_terminal_list rules =
  computed_fixed_point compare_sets wrapper (rules, []);;

let rec filter rules terminal_list new_rules = match rules with
  | [] -> new_rules
  | (lhs, rhs)::xs -> 
  if remove_terminal rhs terminal_list then filter xs terminal_list (new_rules@[(lhs, rhs)])
  else filter xs terminal_list new_rules;;

let filter_blind_alleys g = 
  (fst g), (filter (snd g) (snd (get_terminal_list (snd g))) []);;
