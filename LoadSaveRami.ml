#load "dynlink.cma"
#load "camlp4o.cma"

type token = LPar | RPar | TGen of string;;

#use "MultiEnsemble.ml" ;;
#use "Dictionnaire.ml" ;;
#use "regle.mli" ;;
#use "Lettres.ml" ;;

module LoadSaveRami = functor (R : REGLE) ->
struct

  type t = R.t
  type combi = R.combi
  type main = R.main
  type etat = R.etat
  type joueur = Player of (string * int * bool * main)

  (*
  	Fonction mangeant les espaces dans un stream tant qu'il y en a.
  *)
  let rec mange_Sp = parser
		| [< '' '; s >] -> mange_Sp s
		| [< >] -> () ;;

	(*
		Fonction qui convertit un caractère de type chiffre en entier.
	*)
  let digit c = int_of_char c - int_of_char '0';;

  (*
  	Fonction de conversion d'un stream en entier.
  *)
  let rec horner n = parser
  		| [< ''0'..'9' as c ; m = horner (10 * n + digit c) >] -> m
  		| [< >] -> n;;

  (*
  	Parser d'analyse lexicale d'un TGen.
  *)
  let rec analex_TGen = parser
		| [< ''a'..'z' | 'A'..'Z' | '0'..'9' | '*' as c ; s >] -> (String.make 1 c)^(analex_TGen s)
		| [< >] -> "";;

	(*
		Parser de conversion d'un char stream vers un token stream.
	*)
  let rec analex = parser
		| [< '' ' ; s >] -> [< analex s >]
		| [< ''\n'; s >] -> [< analex s >]
		| [< ''\t'; s >] -> [< analex s >]
		| [< ''\r'; s >] -> [< analex s >]
		| [< ''(' ; s >] -> [< 'LPar; analex s >]
		| [< '')' ; s >] -> [< 'RPar; analex s >] 
		| [< str = analex_TGen; s >] -> [< 'TGen(str); analex s >] ;;

	(*
			Parser de lecture de l'identité d'un joueur.
	*)
  let rec ident = parser
		| [< ''a'..'z'|'A'..'Z' as c ; s >] -> (String.make 1 c)^(ident s)
		| [< >] -> "";;

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

	(*
		
	*)
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



	(** TESTS **)

	(*
		let s = Stream.of_string "Flo" ;;
		ident s ;;
		let s = Stream.of_string "(F A C I L E)" ;;
		analex_TGen "" s ;;
		analex s ;;
		c x ;;
  	let x = analex s ;;
		tl x ;;
		tl s ;;
	*)

end;;
