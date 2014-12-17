#load "dynlink.cma"
#load "camlp4o.cma"

type token = LPar | RPar | TGen of string;;

#use "MultiEnsemble.ml" ;;
#use "Dictionnaire.ml" ;;
#use "regle.mli" ;;
#use "Lettres.ml" ;;

module Sauvegarde = functor (R : REGLE) ->
struct

  type t = R.t
  type combi = R.combi
  type main = R.main
  type etat = R.etat
  type joueur = Player of (string * int * bool * main)

  let rec mange_Sp = parser
		| [< '' '; s >] -> mange_Sp s
		| [< >] -> () ;;

  let digit c = int_of_char c - int_of_char '0';;

  let rec horner n = parser
  		| [< ''0'..'9' as c ; m = horner (10 * n + digit c) >] -> m
  		| [< >] -> n;;

  let rec analex_TGen = parser
		| [< ''a'..'z' | 'A'..'Z' | '0'..'9' | '*' as c ; s >] -> (String.make 1 c)^(analex_TGen s)
		| [< >] -> "";;

  let rec analex = parser
		| [< '' ' ; s >] -> [< analex s >]
		| [< ''\n'; s >] -> [< analex s >]
		| [< ''\t'; s >] -> [< analex s >]
		| [< ''\r'; s >] -> [< analex s >]
		| [< ''(' ; s >] -> [< 'LPar; analex s >]
		| [< '')' ; s >] -> [< 'RPar; analex s >] 
		| [< str = analex_TGen; s >] -> [< 'TGen(str); analex s >] ;;


  let rec ident = parser
		| [< ''a'..'z'|'A'..'Z' as c ; s >] -> (String.make 1 c)^(ident s)
		| [< >] -> "";;

	let s = Stream.of_string "Flo";;
	ident s;;
 (** let s = Stream.of_string "(F A C I L E)" ;;**)
(**analex_TGen "" s ;;
analex s;;**)

  (******************************************************************************)
  (** Fonctions utilisées pour créer une liste de token lisible par lit_valeur **)
  (******************************************************************************)

  let rec tl = parser
		| [< 'TGen(tok); s >] -> (TGen(tok)::(tl s))
		| [< >] -> [];;
  let c = parser
		| [< 'LPar; tokList = tl; 'RPar >] -> LPar :: tokList @ [RPar];;

  let tokenToTuile = parser
		| [< lTuile = c; _ >] -> R.lit_valeur lTuile
		| [< 'TGen(tok); _ >] -> R.lit_valeur (TGen(tok)::[]) ;;

  let rec cl = parser
		| [< m = tokenToTuile; p = cl >] -> (m :: p)
		| [< >] -> [] ;;

  let tokenToCombi = parser
		| [< 'LPar; combi = cl; 'RPar; _ >] -> combi ;;
  
(**  let 
c x;;
  let x = analex s ;;
tl x ;;
tl s ;; **)

  let b = parser
		| [< ''t'; ''r'; ''u'; ''e' >] -> true
		| [< ''f'; ''a'; ''l'; ''s'; ''e' >] -> false ;;

  let j = parser
		| [< ''('; ()=mange_Sp; name = ident; ()=mange_Sp; score = horner 0; ()=mange_Sp; pose = b; ()=mange_Sp; main = tokenToCombi; ()=mange_Sp; '')' >] -> Player(name, score, pose, main)

  let jl = parser
		| [< m = j; p = jl >] -> (m :: p)
		| [< >] -> [] ;;
  
  let s = parser
		| [< ''(';()=mange_Sp;''j';''o';''u';''e';''u';''r';''s';()=mange_Sp; >] -> 



end;;
