module type TJeu =
  functor (Rule: REGLE) -> sig
    val coup_valide : Rule.combi list (* jeu en cours *) -> Rule.main (* main du joueur *)
      -> Rule.combi list (* nouveau jeu *) -> Rule.main (* nouvelle main du joueur *) -> bool (* a posé *) -> bool
    val initialiser : string list -> Rule.etat
    val lit_coup : string -> Rule.main -> Rule.combi list -> bool -> (Rule.main * (Rule.combi list)) option
    val joue : Rule.etat -> (string * int) list
    val sauvegarde : Rule.etat -> string
    val chargement : char Stream.t -> Rule.etat
  end;;
