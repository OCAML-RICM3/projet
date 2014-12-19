(*#load "dynlink.cma"
#load "camlp4o.cma"

#use "Dictionnaire.ml"
#use "regle.mli"
#use "MultiEnsemble.ml"*)

open Dictionnaire
open MultiEnsemble
open Regle

module Lettres =
struct

  type t = char
  type combi = t list
  type main = t MultiEnsemble.mset

  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int} ;;

  let paquet = [ ('A', 8); ('B', 2); ('C', 3); ('D', 3); ('E', 16); ('F', 2); ('G', 2);
 ('H', 2); ('I', 9); ('J', 1); ('K', 1); ('L', 6); ('M', 4); ('N', 7); ('O', 7);
 ('P', 2); ('Q', 1); ('R', 7); ('S', 7); ('T', 7); ('U', 7); ('V', 2); ('W', 1);
 ('X', 1); ('Y', 1); ('Z', 1); ('*', 2)] ;;

  (*
    Type    :   char list -> String
    Rôle    :   Convertit une liste de caractère en une chaine de caractères
    Entrées :   la liste de caractères à convertir
    Sorties :   la chaine de caractère issue de la conversion
  *)
  let rec charListToString (c : char list) =
    match c with
    | [] -> String.make 0 'a'
    | t::q -> (String.make 1 t)^charListToString q;;

  (*
    Type    :   string -> bool
    Rôle    :   Determine si un mot est en majuscules et s'il est valide
    Entrées :   la chaine à analyser
    Sorties :   un booléen, résultat de l'analyse
  *)
  let valide s =
    ((String.length s) <> 0) &&
    begin
      let ret = ref true in
      for i = 0 to (String.length s) - 1 do
        let c = Char.code s.[i] in
        ret := (!ret) && (c >= (Char.code 'A')) && (c <= (Char.code 'Z'))
      done;
      !ret
    end

  (*
    Type    :   combi -> bool
    Rôle    :   Determine si une combinaison est valide
    Entrées :   la combinaison à analyser
    Sorties :   un booléen, résultat de l'analyse
  *)
  let combi_valide (c : combi) : bool = 
    let s = charListToString c in 
    (Dictionnaire.member s (Dictionnaire.dico))
    && (String.length s >= 3) && (valide s) ;;

  (*
    Type    :   combi list -> int
    Rôle    :   Calcul la taille maximale des combinaison possibles
    Entrées :   la liste de combinaisons à analyser
    Sorties :   un entier, résultat de la recherche
  *)
  let nbCombi (c : combi list) : int =
    let rec nbCombiSub c l =
      match c with
      | [] -> l
      | t::q -> let long = List.length t in
        if long > l then
          nbCombiSub q long
        else nbCombiSub q l
    in nbCombiSub c 0;;

  (*
    Type    :   main -> combi -> main
    Rôle    :   Ajoute l'ensemble des lettres présentes dans la combinaison à la main de départ
    Entrées :   une main de départ, une combinaison
    Sorties :   la main finale, union de la main de départ avec la combinaison
  *)
  let rec addWord (m : main)(p : combi) : main =
    let addCar main c = MultiEnsemble.add (c, 1) main in
    match p with
    | [] -> m
    | t::q -> addWord (addCar m t) q ;;

  (*
    Type    :   main -> combi list -> main
    Rôle    :   Ajoute l'ensemble des lettres présentes dans la liste de combinaisons à la main de départ
    Entrées :   une main de départ, une liste de combinaisons
    Sorties :   la main finale, union de la main de départ avec la liste de combinaisons
  *)
  let rec creationMain (m : main)(p : combi list) : main =
    match p with
    | [] -> m
    | t::q -> creationMain (addWord m t) q ;;

  (*
    Type    :   combi list -> bool
    Rôle    :   Test si une liste de combinaisons est valide, 
                  c'est-à-dire que l'ensemble de ses combinaisons soient valides
    Entrées :   la liste de combinaisons à tester
    Sorties :   un booléen, résultat du test
  *)
  let rec listValide (p : combi list) : bool =
    match p with
    | [] -> true
    | t::q -> combi_valide t && listValide q ;;

  (*
    Type    :   main -> combi list -> main -> bool
    Rôle    :   Test si le premier coup est valide, c'est à dire que la liste de combinaisons est valide,
                  et que l'union de la main finale avec la liste de combinaisons est égale à la main de départ
    Entrées :   la main de départ, une liste de combinaisons et la main finale
    Sorties :   un booléen, attestant de la validité ou non du premier coup
  *)
  let premier_coup_valide (m : main)(p : combi list)(n : main) : bool =
    if nbCombi p < 6 then
      false
    else
      let mainInit = creationMain n p in
      MultiEnsemble.eg m mainInit && listValide p ;;

  (*
    Type    :   combi -> combi list -> combi list
    Rôle    :   Retire une combinaison d'une liste de combinaisons
    Entrées :   la combinaison à supprimer, et la liste de combinaisons dans laquelle elle doit être supprimée
    Sorties :   une liste de combinaisons, résultat de la suppression
  *)
  let rec removeWord (w : combi)(p : combi list) : combi list = 
    match p with
    | [] -> []
    | t::q -> if w = t then q else t::(removeWord w q) ;;

  (*
    Type    :   combi list -> combi list -> combi list
    Rôle    :   Retire une liste de combinaisons d'une liste de combinaisons
    Entrées :   la liste de combinaisons à supprimer, et la liste de combinaisons dans laquelle elle doit être supprimée
    Sorties :   une liste de combinaisons, résultat de la suppression
  *)
  let rec removeList (l : combi list)(p : combi list) : combi list =
    match p with
    | [] -> l
    | t::q -> removeList (removeWord t l) q ;;

  (*
    Type    :   combi list -> main -> combi list -> main -> int
    Rôle    :   Calcul le score associé à un jeu en cours, la main d'un joueur, un nouveau jeu et la nouvelle main
                  du joueur.
    Entrées :   une liste de combinaisons (jeu en cours), une main (main actuelle du joueur), une liste de combinaisons
                  (nouveau jeu) et une main (nouvelle main du joueur).
    Sorties :   le score associé au tour de jeu
  *)
  let points (c : combi list (* jeu en cours *))(m : main (* main du joueur *))
	(n : combi list (* nouveau jeu *))
	(nm : main (* nouvelle main du joueur *)) : int =
    let score = ref 0 in
    if nm = [] then
      score := 2*(MultiEnsemble.nbElem m)
    else
      score := MultiEnsemble.nbDifference m nm;
      score := !score + nbCombi (removeList c n);
    !score ;;

  (*
    Type    :   main -> int
    Rôle    :   Calcul les points à l'état final par rapport à une main
    Entrées :   une main
    Sorties :   les points finaux associé à l'état final
    Rem     :   Dans le cas du Rami des Lettres, les points finaux sont de 0
  *)
  let points_finaux (m : main) : int = 0 ;;

  let main_min = 7;;

  let main_initiale = 14;;

  (*
    Type    :   token list -> t
    Rôle    :   Transforme une liste de token en une tuile de type t
    Entrées :   la liste de token à transformer
    Sorties :   la tuile, résultat de la transformation

    Commentaire : Dans le cas du "Rami des Lettres", le seul token valide est celui ne comportant
                    qu'une lettre, les autres ne sont pas valides.
  *)
  let lit_valeur (t : token list) : t = 
    match t with
    | [TGen(tok)] -> if String.length tok > 1 then
        failwith "Erreur de tuile pour le Rami des Lettres"
      else tok.[0]
    | _ -> failwith "Erreur de tuile pour le Rami des Lettres" ;;

  (*
    Type    :   t -> String
    Rôle    :   Stringify une tuile 
    Entrées :   la tuile à transformer en chaine de caractères
    Sorties :   la chaine de caractère résultante de la conversion
  *)
  let ecrit_valeur (t : t) : string = String.make 1 t ;;

  let fin_pioche_vide = true ;;

end;;

  (** TESTS **)
  (*
    let x = [['A';'B'; 'C'; 'D'; 'E'; 'F']; ['T'; 'E'; 'S'; 'T']; ['A'; 'H'; 'X']] ;;
    nbCombi x;;

    let jeu = [['A'; 'R'; 'B'; 'R'; 'E']; ['T'; 'E'; 'S'; 'T']] ;;
    let mainInit = [('A', 1); ('C', 1); ('L', 1); ('M', 1); ('O', 1); ('S', 1); ('T', 3)] ;;
    let nouveauJeu = [['A'; 'R'; 'B'; 'R'; 'E']; ['T'; 'E'; 'S'; 'T'; 'S']; ['O'; 'C'; 'A'; 'M'; 'L']] ;;
    let mainFinale = [('T', 3)] ;;
    points jeu mainInit nouveauJeu mainFinale ;;
    removeWord ['A'; 'H'; 'X'] x ;;
    removeList x [['A'; 'H'; 'X']; ['T'; 'E'; 'S'; 'T']] ;;
    addWord paquet ['A'; 'A'; 'A'; 'A'] ;;
    creationMain [] x;; 
    listValide x ;;
    let m =  [('A', 3); ('B', 1); ('C', 1); ('D', 1); ('E', 2); ('F', 1); ('T', 2); ('S', 1); ('H', 1); ('X', 1)] ;;
    premier_coup_valide m x [('A', 1)] ;;
    let tok = [TGen "A"];;
    lit_valeur tok ;;
  *)
  