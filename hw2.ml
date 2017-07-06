type ('terminal, 'nonterminal) symbol =
   | T of 'terminal
   | N of 'nonterminal;;

(* 1 *)

let rec helper2 rules matched =
   match rules with
   | [] -> []
   | (l,r)::t -> if l = matched then r::(helper2 t matched)
                 else helper2 t matched;;

let helper1 rules = fun matched -> helper2 rules matched;;

let convert_grammar grammar1 =
   match grammar1 with
   | (start_symbol, rules) -> (start_symbol, helper1 rules);;

(* 2 *)

let rec match_disjunction start_symbol rules matching accept derivation frag =
   match matching with
   | [] -> None
   | h::t -> match (match_conjunction rules h accept (derivation @ [(start_symbol,h)]) frag) with
             | None -> match_disjunction start_symbol rules t accept derivation frag
             | a -> a

and match_conjunction rules rule accept derivation frag =
   match rule with
   | [] -> accept derivation frag
   | rh::rt -> match rh with
             | (T t) -> (match frag with 
                        | [] -> None
                        | fh::ft -> (if fh = t 
                                    then match_conjunction rules rt accept derivation ft
                                    else None
				    )
			)
             | (N n) -> match_disjunction n rules (rules n) (match_conjunction rules rt accept) derivation frag;;

let parse_prefix gram accept frag =
   match gram with
   | (start_symbol, rules) -> match_disjunction start_symbol rules (rules start_symbol) accept [] frag;;