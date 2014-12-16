module MultiEnsemble :
sig
  type 'a mset = ('a * int) list

  val vide : 'a mset
  val union : 'a mset -> 'a mset -> 'a mset
  val appart : 'a -> 'a mset -> bool
  val eg : 'a mset -> 'a mset -> bool

  val add : 'a * int -> 'a mset -> 'a mset
  val appart_couple : 'a * int -> 'a mset -> bool
  val remove : 'a * int -> 'a mset -> 'a mset

end;;
