module Dictionnaire :
  sig
    type dico = Noeud of dico array * bool | Feuille
    val dico_vide : unit -> dico
    val member : string -> dico -> bool
    val add : string -> dico -> dico
    val remove : string -> dico -> dico
    val of_stream : char Stream.t -> dico
    val to_list : dico -> string list
  end
