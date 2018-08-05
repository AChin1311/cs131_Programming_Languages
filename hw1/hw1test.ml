let my_subset_test0 = not (subset [1;2;3] [])

let my_equal_sets_test0 = equal_sets [] []

let my_set_union_test0 = equal_sets (set_union [3;1;3] [3;1;3]) [1;3]

let my_set_intersection_test0 = equal_sets (set_intersection [] []) []

let my_set_diff_test0 = equal_sets (set_diff [1;2;3] [1;2]) [3]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 666) 123 = 0

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x / 666) 1 123 = computed_fixed_point (=) (fun x -> x / 666) 123

let my_while_away_test0 = while_away ((+) 3) ((>) 10) 0 = [0; 3; 6; 9]

let my_rle_decode_test0 = (rle_decode [0,"a"; 1,"b"; 2,"c"; 3,"d"]) = ["b"; "c"; "c"; "d"; "d"; "d"]

type grammar_nonterminals =
  | SNP | SVP | PNP | PVP | S | Q

let grammar_rules =
   [S, [N Q];
    S, [N S];
    S, [N SNP; N SVP];
    S, [N PNP; N PVP];
    SNP, [T"snoun"];
    SNP, [T"adj";N SNP];
    PNP, [T"pnoun"];
    PNP, [T"adj";N PNP];
    SVP, [T"sverb"];
    SVP, [N SVP;T"adv"];
    PVP, [T"pverb"];
    PVP, [N PVP;T"adv"]]

let my_filter_blind_alleys_test0 = 
  filter_blind_alleys (S, grammar_rules) = (S, List.tl grammar_rules);;




