type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let rec convert_grammar_helper rules term =
  match rules with
  | [] -> []
  | (lhs, rhs)::rest ->
    if lhs = term then rhs::(convert_grammar_helper rest term)
    else convert_grammar_helper rest term
;;

let convert_grammar gram1 =
  ((fst gram1), fun term -> convert_grammar_helper (snd gram1) term)
;;

(* Write a function parse_prefix gram that returns a matcher for the grammar gram. 
When applied to an acceptor accept and a fragment frag, the matcher must return 
the first acceptable match of a prefix of frag, by trying the grammar rules in 
order; this is not necessarily the shortest nor the longest acceptable match. 
A match is considered to be acceptable if accept succeeds when given a derivation 
and the suffix fragment that immediately follows the matching prefix. When this 
happens, the matcher returns whatever the acceptor returned. If no acceptable 
match is found, the matcher returns None.
 *)

(* Match a rule rhs with frag, return value: None / Some v *)
let rec matcherer rhs rules_fun accept derivation frag =
  match rhs with
  | [] -> accept derivation frag (* finish matching rhs *)
  | item::rest_item ->
    match frag with 
    | [] -> None (* frag ends first *)
    | f::rag -> 
      match item with 
      | T x -> 
        if x = f then matcherer rest_item rules_fun accept derivation rag (* go on match rag with rest_item *)
        else None (* fail to match *)
      | N y -> 
        let alter_lst = rules_fun y in
        let new_accept = matcherer rest_item rules_fun accept in (* construct a new accept that implicitly match the rest of items *)
        matcher y rules_fun alter_lst new_accept derivation frag 

  (* Match each rule in alter_lst to frag *)
  and matcher nonterm rules_fun alter_lst accept derivation frag =
    match alter_lst with (* try each rhs *)
    | [] -> None (* no more rhs *)
    | rhs::rhs_rest ->
      let result = matcherer rhs rules_fun accept (derivation@[nonterm, rhs]) frag in
      match result with 
      | None -> matcher nonterm rules_fun rhs_rest accept derivation frag (* not found, go on to next rhs *)
      | Some x -> Some x (* found a solution *)
  ;;

let parse_prefix gram accept frag = 
  let alter_lst = (snd gram) (fst gram) in
  matcher (fst gram) (snd gram) alter_lst accept [] frag
;;

