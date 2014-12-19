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
  val removeOcc : 'a * int -> 'a mset -> 'a mset
  val difference : 'a mset -> 'a mset -> 'a mset
  val nbElem : 'a mset -> int
  val nbDifference : 'a mset -> 'a mset -> int

  val elemN : 'a mset -> int -> 'a
  val rand : 'a mset -> 'a

end;;
