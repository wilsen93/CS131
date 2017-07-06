let rec contains a b =
   match a with
   | [] -> false
   | h :: t -> if b = h then true
               else contains t b;;
(*1*)
let rec subset a b =
   match a with
   | [] -> true
   | h :: t -> if contains b h then subset t b
               else false;;

(*2*)
let equal_sets a b =
   if subset a b && subset b a then true
   else false;;

(*3*)
let set_union a b = a @ b;;

(*4*)
let rec set_intersection a b =
   match a with
   | [] -> []
   | h :: t -> if contains b h then h :: set_intersection t b
               else set_intersection t b;;

(*5*)
let rec set_diff a b =
   match a with
   | [] -> []
   | h :: t -> if contains b h then set_diff t b
               else h :: set_diff t b;;

(*6*)
let rec computed_fixed_point eq f x =
   if eq (f x) x then x
   else computed_fixed_point eq f (f x);;

(*7*)
let rec computed_periodic_point eq f p x =
   match p with
   | 0 -> x
   | _ -> if eq x (f(computed_periodic_point eq f (p-1) (f x))) then x
          else (computed_periodic_point eq f p (f x));;

(*8*)
let rec while_away s p x =
   if not (p x) then []
   else x :: while_away s p (s x);;

(*9*)
let rec append (a, b) =
   match a with
   | 0 -> []
   | _ -> b :: (append ((a - 1), b));;

let rec rle_decode lp =
   match lp with
   | [] -> []
   | h::t -> set_union (append h) (rle_decode t);;

(*10*)


type ('nonterminal, 'terminal) symbol =
   | N of 'nonterminal
   | T of 'terminal;;

(*This function checks if a symbol is terminable*)

let terminablesymbol symbol terminable = 
   match symbol with
   | T s -> true
   | N s -> if contains terminable s then true
            else false
   | _ -> false;;


(*This function checks if a rule is terminable*)

let rec terminablerule rule terminable =
   match rule with
   | [] -> true
   | h::t -> if terminablesymbol h terminable then terminablerule t terminable
             else false;;

(*This function filters the valid symbols*)
let rec filtersymbols rules terminable =
   match rules with
   | [] -> terminable
   | (symbol, rule) :: t -> if terminablerule rule terminable
                            then filtersymbols t (symbol :: terminable)
                            else filtersymbols t terminable;;

(*This function filters valid rules*)
let rec filterrules rules terminable result =
   match rules with
   | [] -> result
   | (symbol, rule) :: t -> if terminablerule rule terminable 
                            then filterrules t terminable (result @ [symbol, rule])
                            else filterrules t terminable result;;

let equal (a,b) (c,d) = equal_sets b d;;

let helper (rules, terminable) = rules, filtersymbols rules terminable;;

let filter_blind_alleys g =
   match g with
   | (start, rules) -> (start, filterrules rules (snd(computed_fixed_point equal helper (rules, []))) []);;