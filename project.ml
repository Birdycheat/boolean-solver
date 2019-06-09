(*
#use "projet.ml" ;;
*)

type eb = V of int | TRUE | FALSE | AND of eb * eb | OR of eb * eb | XOR of eb * eb | NOT of eb ;;

let rec est_dans e l =
	match l with
	|ee::ll -> 
		if ee = e
			then true
		else est_dans e ll
	|[] -> false;;

let rec effacer_doublons l =
	match l with
	|e::ll-> 
		if est_dans e ll 
			then effacer_doublons ll
		else
			e::effacer_doublons ll
	|[]->[];;

let rec variables l =
	match l with
	|OR(a,b)-> ((variables a)@(variables b))
	|XOR(a,b)-> ((variables a)@(variables b))
	|NOT(a)-> ((variables a))
	|AND(a,b)-> ((variables a)@(variables b))
	|V(v)-> (l::[])
	|_ -> [];;

let rec toutes_variables lebg lebd =
	match (lebg, lebd) with
	|([],[]) -> []
	|(ebg::llebg, ebd::llebd) -> (variables ebg)@(toutes_variables llebg llebd)@(variables ebd)
	|_->[];;

let rec ajouter e l =
	match l with
	|[[]]-> [[e]]
	|ee::ll-> (e::ee)::(ajouter e ll)
	|[]->[];;

let rec partitions l =
	match l with
	|[]-> [[]]
	|e::ll-> let sans = partitions ll in
		let avec = ajouter e sans in
			sans@avec;;

let rec isomorphisme var pa =
	match var with
	|[]->[]
	|v::vvar-> if est_dans v pa
		then (v,TRUE)::(isomorphisme vvar pa)
		else (v,FALSE)::(isomorphisme vvar pa);;

let rec distributions var pvar =
	match pvar with
	|[]->[]
	|pa::ppvar->(isomorphisme var pa)::(distributions var ppvar);;


let ou a b =
	match a with
	|TRUE -> TRUE
	|FALSE -> b
	|_ -> OR(a,b);;

let et a b =
	match a with
	|FALSE -> FALSE
	|TRUE -> b
	|_ -> AND(a,b);;

let non x =
	match x with
	|FALSE -> TRUE
	|TRUE -> FALSE
	|_-> NOT(x);;

let ouexcl a b =
	match a with
	|TRUE -> non b
	|FALSE -> b
	|_-> XOR(a,b);;

let rec eval eb =
	match eb with
	|NOT(x) -> non (eval x)
	|OR(a,b) -> ou (eval a) (eval b)
	|AND(a,b) -> et (eval a) (eval b)
	|XOR(a,b) -> ouexcl (eval a) (eval b)
	|_ -> eb;;

let rec egalite a b =
	if (eval a) = (eval b) then true else false;;

let rec chercher eb dvar =
	match dvar with
	|[]-> FALSE
	|(ee,b)::ddvar -> if ee = eb then b else chercher eb ddvar;;

let rec remplacer eb dvar =
	match eb with
	|V(i) -> chercher eb dvar
	|NOT(x) -> NOT(remplacer x dvar)
	|OR(a,b) ->  OR(remplacer a dvar,remplacer b dvar)
	|AND(a,b) -> AND(remplacer a dvar,remplacer b dvar)
	|XOR(a,b) -> XOR(remplacer a dvar,remplacer b dvar)
	|_ -> eb;;

let rec test lebg lebd pvar =
	match (lebg,lebd) with
	|([],[]) -> true
	|(ebg::llebg, ebd::llebd) -> if (eval (remplacer ebg pvar)) = (eval (remplacer ebd pvar) ) then (test llebg llebd pvar) else false;;

let rec dispatch lebg lebd pvar=
	match pvar with
	|[] -> []
	|vvar::ppvar -> if test lebg lebd vvar = true then vvar::(dispatch lebg lebd ppvar) else dispatch lebg lebd ppvar;;

let main lebg lebd = dispatch lebg lebd (distributions (effacer_doublons(toutes_variables lebg lebd)) (partitions (effacer_doublons (toutes_variables lebg lebd))));;

let a = AND(NOT(V 3),NOT(XOR(V 1, V 2)));;
let c = variables a;;
let b = partitions (c);;
let d = distributions c b;;



let g = [OR(V(1),V(2));XOR(V(1),V(3));NOT(AND(V(1),AND(V(2),V(3))))];;
let d = [TRUE; V(2);TRUE];;


(* Pour extraire nos variables nous allons donc procéder en deux étapes :
	- On extrait toutes les occurrences de symboles et on les insère dans un tableau
	- Le talbeau est ensuite épuré de ses doublons
*)
