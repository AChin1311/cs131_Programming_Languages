type grammar1_nonterminals =
  | S | NP | VP | DT | NN | VB | PP | P

let grammar_rules =
   [S, [N NP; N VP];
    S, [N NP; N VP; N PP];
    NP, [N DT; N NN];
    NP, [N NN];
    DT, [T "a"];
    DT, [T "the"];
    DT, [T "that"];
    NN, [T "I"];
    NN, [T "Daphne"];
    NN, [T "cat"];
    NN, [T "dog"];
    NN, [T "house"];
    NN, [T "pool"];
    VP, [N VB; N NP; N PP];
    VP, [N VB; N NP];
    VP, [N VB];
    VB, [T "see"];
    VB, [T "sees"];
    VB, [T "run"];
    VB, [T "runs"];
    VB, [T "jump"];
    VB, [T "jumps"];
    PP, [N P; N NP];
    P, [T "at"];
    P, [T "in"];
    P, [T "on"];
    ]

let gram = (S, grammar_rules)

let converted_gram = convert_grammar gram

let accept_empty_suffix derivation = function
    |[] -> Some (derivation, [])
    | _ -> None

let rec hasPP derivation =
    match derivation with
    |[] -> false
    |(lhs, rhs)::rest -> 
    if lhs = PP then true
    else hasPP rest 

let accept_PP derivation frag = 
    if hasPP derivation then Some (derivation, frag)
    else None

let test_1 = 
    ((parse_prefix converted_gram accept_empty_suffix ["the";"dog";"runs"]) = 
    Some ( [(S, [N NP; N VP]);(NP, [N DT; N NN]);(DT, [T "the"]);(NN, [T "dog"]);
    (VP, [N VB]);(VB, [T "runs"])], [])) 

let test_2 = 
    ((parse_prefix converted_gram accept_PP ["Daphne";"sees";"a";"dog";"in";"that";"house"]) =
    Some ( [(S, [N NP; N VP]);(NP, [N NN]);(NN, [T "Daphne"]);(VP, [N VB; N NP; N PP]);(VB, [T "sees"]);
    (NP, [N DT; N NN]);(DT, [T "a"]);(NN, [T "dog"]);(PP, [N P; N NP]);(P, [T "in"]);(NP, [N DT; N NN]);
    (DT, [T "that"]);(NN, [T "house"])], []))


