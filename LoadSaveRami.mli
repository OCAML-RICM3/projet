open Regle

module LoadSaveRami :
  functor (R : REGLE) ->
    sig
      type t = R.t
      type combi = R.combi
      type main = R.main
      type etat = R.etat
      type joueur = Player of (string * int * bool * main)
      val fromListToMain : t list -> main
      val mange_Sp : char Stream.t -> unit
      val digit : char -> int
      val horner : int -> string -> int
      val analex_TGen : char Stream.t -> string
      val analex : char Stream.t -> Regle.token Stream.t
      val tl : Regle.token Stream.t -> Regle.token list
      val c : Regle.token Stream.t -> Regle.token list
      val tokenToTuile : Regle.token Stream.t -> R.t
      val cl : Regle.token Stream.t -> R.t list
      val tokenToCombi : Regle.token Stream.t -> R.t list
      val tokenToCombiList : Regle.token Stream.t -> R.t list list
      val ident : Regle.token Stream.t -> string
      val hornerTok : Regle.token Stream.t -> int
      val b : Regle.token Stream.t -> bool
      val j : Regle.token Stream.t -> joueur
      val readMainCombiList : Regle.token Stream.t -> main * R.t list list
      val jl : Regle.token Stream.t -> joueur list
      val joueurs : Regle.token Stream.t -> joueur list
      val jeu : Regle.token Stream.t -> R.t list list
      val pioche : Regle.token Stream.t -> main
      val tour : Regle.token Stream.t -> int
      val getArray :
        joueur list -> string array * int array * bool array * main array
      val s : Regle.token Stream.t -> R.etat
      val getEtat : char Stream.t -> R.etat
    end
