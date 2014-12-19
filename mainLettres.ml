open LoadSaveRami
open Regle
open Lettres
open Jeu

module Rami = Jeu(Lettres)

let _ = Random.self_init ();
Rami.nouvellePartie ["Albert"; "Damien"];;
(*Rami.joue (Rami.chargement(Stream.of_channel(open_in ("jeu.txt"))));;*)