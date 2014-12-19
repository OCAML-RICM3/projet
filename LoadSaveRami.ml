(*#load "dynlink.cma"*)
(*#load "camlp4o.cma"*)

(*#use "MultiEnsemble.ml" ;; *)
(*#use "regle.mli" ;;*)
(*#use "Lettres.ml" ;;*)

open MultiEnsemble
open Regle

module LoadSaveRami = functor (R : REGLE) ->
struct
  type t = R.t
  type combi = R.combi
  type main = R.main
  type etat = R.etat
  type joueur = Player of (string * int * bool * main)

  (*
		Type    :   t list -> main
		Rôle    :   Transforme une liste de R.t en une main
		Entrées :   la liste à transfomer
		Sorties :   la main résultante
  *)
  let rec fromListToMain (l : t list) : main =
	match l with
	| [] -> MultiEnsemble.vide
	| t::q -> MultiEnsemble.add (t, 1) (fromListToMain q) ;;

  (*
		Type    :   char Stream.t -> unit
		Rôle    :   Fonction mangeant les espaces dans un stream tant qu'il y en a.
		Entrées :   le stream à parser
		Sorties :   le stream parsé
  *)
  let rec mange_Sp = parser
		| [< '' '; s >] -> mange_Sp s
		| [< >] -> () ;;

	(*
		Type    :   char -> int
		Rôle    :   Fonction qui convertit un caractère de type chiffre en entier.
		Entrées :   un caractère
		Sorties :   l'entier associé
  *)
  let digit c = int_of_char c - int_of_char '0';;
	
	(*
		Type    :   string -> int
		Rôle    :   Fonction de conversion d'un string en entier.
		Entrées :   une chaine
		Sorties :   l'entier associé
  *)
  let rec horner(n : int)(s : string) : int =
		if String.length s > 0 then
	  	horner ( 10*n + digit s.[0]) (String.sub s 1 ((String.length s)-1))
		else n ;;

	(*
		Type    :   char Stream.t -> string
		Rôle    :   Parser d'analyse lexicale d'un TGen.
		Entrées :   un stream de caractères
		Sorties :   une chaine de caractères extraite du stream
  *)
  let rec analex_TGen = parser
		| [< ''a'..'z' | 'A'..'Z' | '0'..'9' | '*' as c ; s >] -> (String.make 1 c)^(analex_TGen s)
		| [< >] -> "";;

	(*
		Type    :   char Stream.t -> token Stream.t
		Rôle    :   Parser de conversion d'un stream de caractères vers un stream de token.
		Entrées :   un stream de caractères
		Sorties :   le stream de token associé
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
		Type    :   token Stream.t -> token list
		Rôle    :   Extrait une liste de token d'un stream
		Entrées :   un stream de caractères
		Sorties :   la liste de token extraite
  *)
  let rec tl = parser
		| [< 'TGen(tok); s >] -> (TGen(tok))::(tl s)
		| [< >] -> [];;

	(*
		Type    :   token Stream.t -> token list
		Rôle    :   Extrait une liste de token d'un stream, tel que le stream est bien parenthésé.
		Entrées :   un stream de caractères
		Sorties :   la liste de token extraite
  *)
  let c = parser
		| [< 'LPar; tokList = tl; 'RPar >] -> LPar :: tokList @ [RPar];;

	(*
		Type    :   token Stream.t -> t
		Rôle    :   Extrait une liste de token d'un stream est le transforme en un élement de type R.t
									si la liste de token est valide (utilisation de R.lit_valeur).
		Entrées :   un stream de caractères
		Sorties :   la tuile de type R.t
  *)
  let tokenToTuile = parser
		| [< lTuile = c; _ >] -> R.lit_valeur lTuile
		| [< 'TGen(tok); _ >] -> R.lit_valeur (TGen(tok)::[]) ;;

	(*
		Type    :   token Stream.t -> combi
		Rôle    :   Extrait une combinaison d'un stream
		Entrées :   un stream de caractères
		Sorties :   la combinaison extraite
  *)
  let rec cl = parser
		| [< m = tokenToTuile; p = cl >] -> (m :: p)
		| [< >] -> [] ;;

	(*
		Type    :   token Stream.t -> combi
		Rôle    :   Extrait une combinaison d'un stream est vérifie que le stream est bien parenthésé.
		Entrées :   un stream de caractères
		Sorties :   la combinaison extraite
  *)
  let tokenToCombi = parser
		| [< 'LPar; combi = cl; 'RPar>] -> combi ;;

	(*
		Type    :   token Stream.t -> combi list
		Rôle    :   Extrait une liste de combinaisons d'un stream
		Entrées :   un stream de caractères
		Sorties :   la liste de combinaisons extraite
  *)
  let rec tokenToCombiList = parser
		| [< combi = tokenToCombi; s >] -> combi::(tokenToCombiList s)
		| [< >] -> [] ;;

	(*
		Type    :   token Stream.t -> string
		Rôle    :   Parse un stream de token en l'identité d'un joueur
		Entrées :   un stream de caractères
		Sorties :   l'identité d'un joueur
  *)
  let ident = parser
		| [< 'TGen(name) >] -> name ;;

	(*
		Type    :   token Stream.t -> int
		Rôle    :   Parse un stream de token en un score
		Entrées :   un stream de caractères
		Sorties :   le score extrait
  *)
  let hornerTok = parser
		| [< 'TGen(score) >] -> horner 0 score ;;

	(*
		Type    :   token Stream.t -> bool
		Rôle    :   Parse un stream de token en l'état de pose ou non d'un joueur
		Entrées :   un stream de caractères
		Sorties :   un booléen, résultat de l'extraction
  *)
  let b = parser
		| [< 'TGen(bool) >] -> bool = "true" ;;

	(*
		Type    :   token Stream.t -> joueur
		Rôle    :   Parse un stream de token en un joueur
		Entrées :   un stream de caractères
		Sorties :   un joueur
  *)
  let j = parser
		| [< 'LPar; name = ident; score = hornerTok; pose = b; main = tokenToCombi; 'RPar >] -> Player(name, score, pose, fromListToMain(main))
	

	(*
		Type    :   token Stream.t -> (combi * combi list)
		Rôle    :   Parse une entrée de l'utilisateur quand celui-ci veut poser des tuiles
		Entrées :   un stream de caractères
		Sorties :   un couple formé à partir de la nouvelle main du joueur, et de l'ensemble des combinaisons
									présentes sur la table
  *)
	let readMainCombiList = parser
		| [< main = tokenToCombi; l = tokenToCombiList >] -> (fromListToMain(main), l)

	(*
		Type    :   token Stream.t -> joueur list
		Rôle    :   Parse un stream de token en une liste de joueurs
		Entrées :   un stream de caractères
		Sorties :   une liste de joueurs
  *)
  let rec jl = parser
		| [< m = j; p = jl >] -> (m :: p)
		| [< >] -> [];;
  
  (*
		Type    :   token Stream.t -> joueur list
		Rôle    :   Parse un stream de token en une liste de joueurs, et vérifie la bonne mise en forme du bloc "joueur"
		Entrées :   un stream de caractères
		Sorties :   une liste de joueurs
  *)
  let joueurs = parser
		| [< 'LPar; 'TGen(jStr); players = jl; 'RPar; _ >] -> 
		  if jStr = "joueurs" then players
		  else failwith "Fichier invalide : erreur de syntaxe" ;;

	(*
		Type    :   token Stream.t -> combi list
		Rôle    :   Extrait la liste de combinaisons dans le jeu, et vérifie la bonne mise en forme du bloc "jeu"
		Entrées :   un stream de caractères
		Sorties :   la liste de combinaisons extraite
  *)
  let jeu = parser
		| [< 'LPar; 'TGen(gStr); game = tokenToCombiList; 'RPar; _ >] -> 
		  if gStr = "jeu" then game
		  else failwith "Fichier invalide : erreur de syntaxe" ;;

	(*
		Type    :   token Stream.t -> combi
		Rôle    :   Extrait une combinaison représentative de la pioche, et vérifie la bonne mise en forme du bloc "pioche"
		Entrées :   un stream de caractères
		Sorties :   la pioch
  *)
  let pioche = parser
		| [< 'LPar; 'TGen(pStr); paquet = cl; 'RPar; _ >] -> 
		  if pStr = "pioche" then fromListToMain paquet
		  else failwith "Fichier invalide : erreur de syntaxe" ;;

	(*
		Type    :   token Stream.t -> int
		Rôle    :   Extrait le nombre de tours déjà joué, et vérifie la bonne mise en forme du bloc "tour"
		Entrées :   un stream de caractères
		Sorties :   le nombre de tours
  *)
  let tour = parser
		| [< 'LPar; 'TGen(tStr); nb = hornerTok; 'RPar >] -> 
		  if tStr = "tour" then nb
		  else failwith "Fichier invalide : erreur de syntaxe" ;;

	(*
		Type    :   joueur list -> (string array * int array * bool array * main array)
		Rôle    :   Transforme une liste de joueurs en un objet composé des différents identificateur de joueur
									sous forme de tableaux.
		Entrées :   une liste de joueurs
		Sorties :   un objet représentatif de l'ensemble des données de joueurs
  *)
  let rec getArray (l : joueur list) : (string array * int array * bool array * main array) =
	match l with
	| [] -> (Array.make 0 "", Array.make 0 0, Array.make 0 true, Array.make 0 [])
	| Player(n, s, p, m)::q -> let (n1, s1, p1, m1) = getArray q in
	   (Array.append (Array.make 1 n) n1, 
	   	Array.append (Array.make 1 s) s1, 
	   	Array.append (Array.make 1 p) p1, 
	   	Array.append (Array.make 1 m) m1) ;;

	(*
		Type    :   token Stream.t -> etat
		Rôle    :   Récupère un état à partir d'un stream de token
		Entrées :   un stream de token
		Sorties :   l'état représentatif du stream de token passé en paramètre
  *)
  let s = parser
		| [< player = joueurs; game = jeu; paquet = pioche; t = tour >] ->
		  let (names, scores, poses, mains) = getArray player in
		  {	R.noms = names; 
			R.scores = scores; 
			R.mains = mains; 
			R.table = game; 
			R.pioche = paquet; 
			R.pose = poses; 
			R.tour = t} ;;

	(*
		Type    :   char Stream.t -> etat
		Rôle    :   Récupère un état à partir d'un stream de char
		Entrées :   un stream de token
		Sorties :   l'état représentatif du stream de token passé en paramètre
  *)
  let getEtat stream = 
		s (analex stream) ;;

end;;

	  
	(** TESTS **)

	(*
		let s = Stream.of_string "Flo" ;;
		ident (analex s) ;;
		let b1 = Stream.of_string "false" ;;
		b (analex b1) ;;
		let score = Stream.of_string "958" ;;
		hornerTok (analex score) ;;
		let s = Stream.of_string "(F A C I L E)" ;;
		analex_TGen "" s ;;
		analex s ;;
		c x ;;
		let x = analex s ;;
		tl x ;;
		tl s ;;


		let tokStream = Stream.of_string "S P O I N E L *";;
		let tokStream2 = Stream.of_string "(S P O I N E L *)";;
		tl (analex tokStream);;
		c (analex tokStream);;
		tokenToTuile (analex tokStream) ;;
		fromListToMain(tokenToCombi (analex tokStream2)) ;;

		let tokStream3 = Stream.of_string "(jeu (F A C I L E)(E X A M E N)(C A * T O N)(E L E V E)(B R I L L A N T))";;
		jeu (analex tokStream3) ;;

		let tokStream4 = Stream.of_string "(pioche C S N O H I N U A E L T M X)";;
		pioche (analex tokStream4) ;;

		let tokStream5 = Stream.of_string "(tour 3)" ;;
		tour (analex tokStream5) ;;


		let playersStream = Stream.of_string "(joueurs (Pascal 17 true (S P O I N E L *)) (Laurent 42 true (N S A V U L G I O)) (Marion 0 false (E E I N M Z S O O V C N L)))" ;;

		getArray(joueurs (analex playersStream)) ;;

		let game = Stream.of_string "(joueurs (Pascal 17 true (S P O I N E L *)) (Laurent 42 true (N S A V U L G I O)) (Marion 0 false (E E I N M Z S O O V C N L)))(jeu (F A C I L E)(E X A M E N)(C A * T O N)(E L E V E)(B R I L L A N T))(pioche C S N O H I N U A E L T M X)(tour 3)" ;;
		getEtat game;;	
	*)

