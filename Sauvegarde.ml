#load "dynlink.cma"
#load "camlp4o.cma"

type token = LPar | RPar | TGen of string;;

#use "MultiEnsemble.ml" ;;
#use "Dictionnaire.ml" ;;
#use "regle.mli" ;;
#use "Lettres.ml" ;;

module Sauvegarde = 
	functor (R : REGLE) ->
struct

  type t = R.t
  type combi = R.combi
  type main = R.main
  type etat = R.etat
  type joeuru = Player of (string * int * bool * main)

  let rec mange_Sp = parser
		| [< '' '; s >] -> mange_Sp s
		| [< >] -> () ;;

  let digit c = int_of_char c - int_of_char '0';;

  let rec horner n = parser
  		| [< ''0'..'9' as c ; m = horner (10 * n + digit c) >] -> m
  		| [< >] -> n;;

  let rec analex_TGen l = parser
		| [< ''a'..'z' | 'A'..'Z' | '0'..'9' | '*' as c ; m = analex_TGen (l^(String.make 1 c)) >] -> m
		| [< >] -> l;;

  let rec analex = parser
		| [< '' ' ; s >] -> [< [< >]; analex s >]
		| [< ''\n'; s >] -> [< [< >]; analex s >]
		| [< ''\t'; s >] -> [< [< >]; analex s >]
		| [< ''\r'; s >] -> [< [< >]; analex s >]
		| [< ''(' ; s >] -> [< 'LPar; analex s >]
		| [< '')' ; s >] -> [< 'RPar; analex s >] 
		| [< str = analex_TGen ""; s >] -> [< 'TGen(str); analex s >]
		| [< >] -> [< >];;


  let rec ident l = parser
		| [< ''a'..'z'|'A'..'Z' as c ; m = ident (l^String.make 1 c) >] -> m
		| [< >] -> l;;

	(**let s = Stream.of_string "Flo";;
	ident (String.make 0 'a') s;;**)
  let s = Stream.of_string "facile" ;;
analex_TGen "" s ;;
analex s;;

  let rec tl = parser
		| [< 'TGen(t); s >] -> (TGen(t)::(tl s))
		| [< >] -> [];;

  let x = analex s ;;
tl x ;;

tl s ;;

  let c = parser
		| [< ''('; ()=mange_Sp; t = tl; ()=mange_Sp; '')' >] -> tl;;

  let rec cl = parser
		| [< m = c; p = cl >] -> (m :: p);;

  let b = parser
		| [< ''t'; ''r'; ''u'; ''e' >] -> true
		| [< ''f'; ''a'; ''l'; ''s'; ''e' >] -> false ;;

  let j = parser
		| [< ''('; ()=mange_Sp; name = ident ""; ()=mange_Sp; score = horner 0; ()=mange_Sp; pose = b; main = c >] -> Player(name, score, pose, main)

  let jl = parser
		| [< m = j; p = jl >] -> (m :: p)



end;;
