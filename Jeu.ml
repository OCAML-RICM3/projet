
(*#load "dynlink.cma";;*)
(*#load "camlp4o.cma";;*)

(*#use "Dictionnaire.ml"*)
(*#use "regle.mli"*)
(*#use "MultiEnsemble.ml"*)
(*#use "Lettres.ml"*)
(*#use "LoadSaveRami.ml"*)

open MultiEnsemble
open LoadSaveRami
open Regle
open Lettres

module Jeu = functor (R : REGLE) ->
struct
  (*module R = Lettres*)
  module ParserLoadSave = LoadSaveRami(R)

  exception CoupValide of string
  exception Save of string
  exception Load of string
  exception GameEnd  

  (*
    Retire une combinaison d'une liste de combinaisons
  *)
  let rec removeCombi (c : R.combi)(l : R.combi list) : R.combi list =
    match l with
    | [] -> []
    | t::q -> if c = t then q
      else t::(removeCombi c q) ;;

  (*
    Fait la différence entre deux listes de combinaisons
  *)
  let rec differenceCombiList (a : R.combi list)(b : R.combi list) : R.combi list =
    match b with
    | [] -> a
    | t::q -> differenceCombiList (removeCombi t a) q ;; 

  (*
    Convertit une combinaison en une main
  *)
  let rec combiToME(l : R.combi) : R.main =
    match l with
    | [] -> []
    | t::q -> MultiEnsemble.add (t, 1) (combiToME q) ;;

  (*
    Convertit une liste de combinaisons en une main
  *)
  let rec combiListToME (l : R.combi list) : R.main =
    match l with
    | [] -> []
    | t::q -> MultiEnsemble.union (combiToME t) (combiListToME q) ;;

  (*
    Test si une liste de combinaison est valide
  *)
  let rec combiListValide (l : R.combi list) : bool =
    match l with
    | [] -> true
    | t::q -> (R.combi_valide t) && combiListValide q ;;

  (*
    Test si un coup donné par l'utilisateur est valide, est lève différentes exceptions suivant les erreurs
  *)
  let coup_valide (game : R.combi list (* jeu en cours *))(main : R.main (* main du joueur *))
      (newGame : R.combi list (* nouveau jeu *))(newMain : R.main (* nouvelle main du joueur *))(pose : bool (* a posé *)) : bool =
    if main = newMain then
      raise (CoupValide "Vous n'avez pas posé de tuiles.")
    else
      let combiPose = differenceCombiList newGame game in
      if pose then
      	  let newGameME = combiListToME newGame and
	      gameMe = combiListToME game in
      	  let gamePose = MultiEnsemble.difference newGameME gameMe and 
	      joueurPose = MultiEnsemble.difference main newMain in
      	  if MultiEnsemble.eg gamePose joueurPose then
            if combiListValide combiPose then true
            else raise (CoupValide "Le coup n'est pas valide.")
      	  else
      	    raise (CoupValide "Les lettres posees ne correspondent pas avec celles que vous avez dans votre main initiale.")
      else
      	if (MultiEnsemble.nbElem main) <> R.main_initiale then
      	  raise (CoupValide "Le nombre de tuiles presentes dans votre main n'est pas valide.")
      	else
          if R.premier_coup_valide main combiPose newMain then true
          else raise (CoupValide "Le premier coup n'est pas valide.");;

  (*
    Pioche un élément à une pioche donnée, et renvoyé la tuile piochée et la nouvelle pioche
  *)
  let piocher (p : R.main) : (R.t * R.main) =
    let tuile = MultiEnsemble.rand p in
    let newPioche = MultiEnsemble.removeOcc (tuile, 1) p in
    (tuile, newPioche) ;;

  (*
    Pioche n éléments à une pioche, renvoit la main crée ainsi que la nouvelle pioche
  *)
  let piocherN (p : R.main)(n : int) : (R.main * R.main) =
    let rec piocherNSub (p : R.main)(n : int) : (R.t list * R.main) = 
      if n > 0 then 
	let (t, np) = piocher p in
	let (t2, np2 ) = piocherNSub np (n-1) in
	(t::t2, np2)
      else ([], p)
    in 
    let (lt, np) = piocherNSub p n in (combiToME lt, np) ;;

  (*
    Initialise l'ensemble des mains sous forme de tableau, en piochant 14 éléments pour chaque mains dans la pioche
      On passe en paramètre la pioche ainsi que le nombre de mains à créer.
  *)
  let rec piocherInit (p : R.main)(n : int) : (R.main array * R.main) =
    if n > 0 then
      let (main, newPaquet) = piocherN p 14 in
      let (main2, np) = piocherInit newPaquet (n-1) in
      ((Array.append (Array.make 1 main) main2), np)
    else
      ((Array.make 0 []), p) ;;

  (*
    Initialise une partie à partir d'une liste de nom de joueurs
  *)
  let initialiser (l : string list) : R.etat =
    let (mains, pioche) = (piocherInit (R.paquet) (List.length l)) in
    {R.noms = Array.of_list l;R.scores = Array.make (List.length l) 0;R.mains = mains;R.table = [];R.pioche = pioche;R.pose = Array.make (List.length l) false;R.tour = 1} ;;

  (*
    Convertit une combinaison en une chaine de caractères.
  *)
  let rec combiToString (c : R.combi) : string =
    match c with
    | [] -> ""
    | [x] -> R.ecrit_valeur x
    | t::q -> (R.ecrit_valeur t) ^ " " ^ (combiToString q) ;;

  (*
    Convertit une liste de combinaisons en une chaine de caractères.
  *)
  let rec combiListToString (l : R.combi list) : string =
    match l with
    | [] -> ""
    | t::q -> "(" ^ (combiToString t)  ^ ")\n" ^ (combiListToString q) ;;

  (*
    Convertit une main en une chaine de caractères.
  *)
  let rec mainToString (m : R.main) : string =
    match m with
    | [] -> ""
    | [(v, 1)] -> R.ecrit_valeur v 
    | (v, nb)::q -> if nb = 1 then 
	(R.ecrit_valeur v) ^ " " ^ (mainToString q)
      else
  	(R.ecrit_valeur v) ^ " " ^ (mainToString ((v, (nb-1))::q));;

  (*
    Convertit un état en une chaine de caractères.
  *)
  let sauvegarde (e : R.etat) : string =
    let str = ref "(joueur \n" in
    for i=0 to (Array.length e.R.noms)-1 do
      str := !str ^ "(" ^ e.R.noms.(i) ^ " " ^ (string_of_int(e.R.scores.(i)))  ^ " " ^ string_of_bool(e.R.pose.(i)) ^ " (" ^ mainToString (e.R.mains.(i)) ^ "))\n";	
    done;
    str := !str ^ "(jeu \n" ^ combiListToString (e.R.table) ^ ")\n(pioche \n" ^ mainToString(e.R.pioche) ^ ")\n(tour " ^ string_of_int(e.R.tour) ^ ")";
    !str;;

  (*
    Fonction de parsing d'un stream de caractères vers un état
  *)
  let chargement (s : char Stream.t) : R.etat =
    ParserLoadSave.getEtat s;;

  let lit_coup (name : string)(main : R.main)(game : R.combi list)(pose : bool) =
    print_endline ("\n ##### " ^ name ^ " #####\n Déjà posé : " ^ string_of_bool pose); 
    print_endline (" Main : " ^ mainToString main ^ "\n");
    print_endline (" Combinaisons en Jeu : \n" ^ combiListToString game);
    print_endline (" Actions possibles : \n - Passer son tour (p)\n - Poser\n");
    let inRead = read_line() in
    if inRead = "p" then
      None
    else
      if inRead = "exit" then
    	 raise GameEnd
      else if inRead = "save" then
	     raise (Save "save.txt")
      else
	let combi = ParserLoadSave.readMainCombiList (ParserLoadSave.analex (Stream.of_string inRead)) in
	Some(combi);;

  let changerJoueur (j : int)(nbJ : int) =
    if j + 1 > nbJ then
	1
      else j + 1;;

  let passerSonTour (e : R.etat) : R.etat =
    let nbJoueurs = Array.length e.R.noms in
    let (t, nP) = piocher (e.R.pioche) in
    let joueur = e.R.tour in
    e.R.mains.(joueur-1) <- MultiEnsemble.add (t, 1) e.R.mains.(joueur-1);
    {
      R.noms = e.R.noms;
      R.scores = e.R.scores;
      R.mains = e.R.mains;
      R.table = e.R.table;
      R.pioche = nP;
      R.pose = e.R.pose;
      R.tour = changerJoueur joueur nbJoueurs
    } ;;

  let jouerUnCoup (e : R.etat)(nMain : R.main)(nTable : R.combi list) : R.etat =
    let joueur = e.R.tour in
    if (coup_valide (e.R.table) (e.R.mains.(joueur-1)) nTable nMain (e.R.pose.(joueur-1))) then
      if ((MultiEnsemble.nbElem nMain) < R.main_min) && (e.R.pioche <> []) then
          let (nMain2, nPioche) = piocherN e.R.pioche (R.main_min-(MultiEnsemble.nbElem nMain)) in
          let _ = e.R.mains.(joueur-1) <- nMain2; in
          {
            R.noms = e.R.noms;
            R.scores = e.R.scores;
            R.mains = e.R.mains;
            R.table = nTable;
            R.pioche = nPioche;
            R.pose = e.R.pose;
            R.tour = changerJoueur joueur (Array.length e.R.noms);
          }
      else
      let _ = e.R.mains.(joueur-1) <- nMain; in
    {
      R.noms = e.R.noms;
      R.scores = e.R.scores;
      R.mains = e.R.mains;
      R.table = nTable;
      R.pioche = e.R.pioche;
      R.pose = e.R.pose;
      R.tour = changerJoueur joueur (Array.length e.R.noms);
    }
    else
      failwith "Erreur de main" ;;

  let rechercheMainVide (m : R.main array) : bool =
    let b = ref false in
    for i=0 to (Array.length m)-1  do
      b := !b || (m.(i) = [])
    done;
    !b ;;

  let finDePartie (e : R.etat) : bool =
    R.fin_pioche_vide && rechercheMainVide (e.R.mains)

  let rec createScoreList (e : R.etat) : (string * int) list =
    let l = ref [] in
    for i=0 to (Array.length (e.R.noms))-1 do
      l := !l@[(e.R.noms.(i), e.R.scores.(i))]
    done;
    !l ;;

  let rec joue (e : R.etat) : (string * int) list =
    let joueursArray = e.R.noms and
	mainsArray = e.R.mains and
	table = e.R.table and
	poseArray = e.R.pose and
	tour = e.R.tour in
    try
      if finDePartie e then
        createScoreList e
      else 
        let newEtat = lit_coup (joueursArray.(tour-1)) (mainsArray.(tour-1)) table (poseArray.(tour-1)) in
        match newEtat with
        | None -> joue (passerSonTour e)
        | Some(nMain, nTable) -> joue (jouerUnCoup e nMain nTable)
    with
    | CoupValide(erreur) -> print_endline ("\n" ^ erreur ^ "\n"); joue e 
    | Save(file) -> print_endline ("Sauvegarde de la partie dans le fichier : " ^ file ^ "\n"); 
      begin
        let save = "jeu.txt"
        in let out_channel = open_out save
        in output_string out_channel (sauvegarde e);
        close_out out_channel;
        joue e;
        end
    | Load(file) -> print_endline ("Chargement de la partie du fichier : " ^ file ^ "\n"); joue e
    | GameEnd -> print_endline ("Arrêt du jeu"); [];;

  let nouvellePartie strList = joue (initialiser strList) ;;

  (*nouvellePartie ["Mathias"; "Matthieu"] ;;*)

end;;

 (** TESTS **)

(*piocher R.paquet ;;
piocherN R.paquet 100 ;;
piocherInit R.paquet 4 ;;
let strJoueurs = ["Florent"; "Germain"; "Albert"; "Gertrude"; "Mathilde"] ;;
initialiser strJoueurs ;;
mainToString R.paquet ;;
combiToString ['A';'B';'C'] ;; 
combiListToString [['A'; 'B']; ['C']; ['T'; 'E'; 'S'; 'T']] ;;
  print_endline (sauvegarde (initialiser ["Florent"; "Germain"; "Albert"; "Gertrude"; "Mathilde"])) ;;

passerSonTour(passerSonTour(passerSonTour(passerSonTour(passerSonTour (initialiser strJoueurs))))) ;;
createScoreList (initialiser strJoueurs) ;;
lit_coup "Florent" Lettres.paquet [['T'; 'E'; 'S'; 'T']; ['M'; 'A'; 'T'; 'T'; 'H'; 'I'; 'E'; 'U']] false ;; *)


