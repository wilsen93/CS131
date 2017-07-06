let my_subset_test0 = subset [1;2;3] [1;2;3;4]
let my_subset_test1 = subset [1;2] [1;2]
let my_subset_test2 = subset [] [1;2;3]
let my_subset_test3 = not (subset [1;2] [])
let my_subset_test4 = not (subset [1;2] [2])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;2] [2;1]
let my_equal_sets_test2 = not (equal_sets [1;3] [1])

let my_set_union_test0 = equal_sets (set_union [1] [2]) [1;2]
let my_set_union_test1 = equal_sets (set_union[3;1] [1;3;2]) [1;2;3;2]
let my_set_union_test2 = not (equal_sets (set_union [1;3] [1]) [1])

let my_set_intersection_test0 = equal_sets (set_intersection [1] [2]) []
let my_set_intersection_test1 = equal_sets (set_intersection [3;1] [1;3;2]) [1;3]

let my_set_diff_test0 = equal_sets (set_diff [1] [2]) [1]
let my_set_diff_test1 = equal_sets (set_diff [3;1;2] [1;3]) [2]
let my_set_diff_test2 = not (equal_sets (set_diff [1;3] [1]) [1])

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x/2) 5000 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) sqrt 100. = 1.

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x/2) 0 (-1) = -1

let my_rle_decode_test0 = equal_sets (rle_decode [2,1; 2,2]) [1;1;2;2]

let my_while_away_test0 = equal_sets (while_away ((+) 3) ((>) 10) 0) [0;3;6;9]

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  filter_blind_alleys awksub_grammar = awksub_grammar

let awksub_test1 =
  filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

let awksub_test2 =
  filter_blind_alleys (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Binop, [T"+"]; Binop, [T"-"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
  = (Expr,
     [Expr, [N Num];
      Expr, [N Expr; N Binop; N Expr];
      Incrop, [T"++"]; Incrop, [T"--"];
      Binop, [T "+"]; Binop, [T "-"];
      Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]])

let awksub_test3 =
  filter_blind_alleys (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    filter_blind_alleys (Expr, List.tl (List.tl awksub_rules))

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let my_filter_blind_alley_test0 =
  filter_blind_alleys giant_grammar = giant_grammar

let my_filter_blind_alley_test1 =
  filter_blind_alleys (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])

let my_filter_blind_alley_test2 =
  filter_blind_alleys (Sentence, List.tl (List.tl (snd giant_grammar))) =
    (Sentence,
     [Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Grunt]; Sentence, [N Shout]])