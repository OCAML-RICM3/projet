module type MultiEnsemble =
sig
  type 'a mset = ('a * int) list

  val vide : 'a mset
  val union : 'a mset -> 'a mset -> 'a mset
  val appart : 'a -> 'a mset -> bool
  val eg : 'a mset -> 'a mset -> bool

end;;
