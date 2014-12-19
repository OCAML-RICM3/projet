open LoadSaveRami
open Regle
open Lettres
open Jeu

module Rami = Jeu(Lettres)

(*
	Type    :   (string * int) list -> string
	Rôle    :   Stringify une liste de scores
	Entrées :   une liste de scores
	Sorties :   la chaine de caractère résultante
*)
let rec afficheScore (l : (string*int) list) =
	match l with
	| [] -> ""
	| (name, score)::q -> name ^ " a réalisé : " ^ (string_of_int score) ^ "points\n"^(afficheScore q);;

let _ = Random.self_init ();
print_endline (" ==================================================================");
print_endline (" |                        Rami des Lettres                        |");
print_endline (" |                     ======================                     |");
print_endline (" | Actions possibles :                                            |");
print_endline (" | - Passer son tour (p)                                          |");
print_endline (" | - Poser (donner sa main ainsi que l'ensemble des combinaisons) |");
print_endline (" | - Sauvegarder (save)                                           |");
print_endline (" | - Quitter (exit)                                               |");
print_endline (" ==================================================================");
(*Rami.nouvellePartie ["Albert"; "Damien"];;*)
let score = Rami.joue (Rami.chargement(Stream.of_channel(open_in ("jeu.txt")))) in
print_endline (afficheScore score);;