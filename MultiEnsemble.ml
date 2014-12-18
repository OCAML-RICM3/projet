module MultiEnsemble =
struct

  type 'a mset = ('a * int) list

  let vide = []

  (*
    Type    :   'a * int -> 'a mset -> 'a mset
    Rôle    :   Cette fonction ajoute un élément associé à un certain nomnbre d'occurrence, dans un multiensemble
    Entrées :   un couple (v, nb) à insérer dans le multiensemble a
    Sorties :   un multiensemble, résultat de l'insertion de (v, nb) dans a
  *)
  let rec add ((v, nb) : 'a * int)(a : 'a mset) : 'a mset =
    match a with
    | [] -> [(v, nb)]
    | t::q -> let (v1, nb1) = t in if v = v1 then (v, (nb+nb1))::q
      else t::(add (v, nb) q)

  (*
    Type    :   'a mset -> 'a mset -> 'a mset
    Rôle    :   Cette fonction calcul l'union de deux multiensembles
    Entrées :   deux multiensemble a et b
    Sorties :   un multiensemble, résultat de l'union de a et b
  *)
  let rec union (a : 'a mset)(b : 'a mset) : 'a mset = 
    match a with 
    | [] -> b
    | t::q -> union q (add t b)
      
  (*
    Type    :   'a -> 'a mset -> bool
    Rôle    :   Cette fonction recherche la présence d'un élément dans un multiensemble et renvoi un booléen
                  en fonction du résultat de la recherche
    Entrées :   l'élément à chercher, dans le multiensemble donné a
    Sorties :   un booléen, résultat de la recherche
  *)
  let rec appart (x : 'a)(a : 'a mset) : bool =
    match a with
    | [] -> false
    |t::q -> let (v, nb) = t in (x = v) || (appart x q)

  (*
    Type    :   'a * int -> 'a mset -> bool
    Rôle    :   Cette fonction recherche la présence d'un élément dans un multiensemble et renvoi un booléen
                  en fonction du résultat de la recherche. Cette version tient compte du nombre d'occurrence
                  de l'élément donné à chercher.
    Entrées :   l'élément à chercher ainsi que sa multiplicité, dans le multiensemble donné a
    Sorties :   un booléen, résultat de la recherche
  *)
  let rec appart_couple (x : 'a * int)(a : 'a mset) :  bool =
    match a with
    | [] -> false
    | t::q -> (x = t) || (appart_couple x q)

  (*
    Type    :   'a * int -> 'a mset -> 'a mset
    Rôle    :   Cette fonction retire un élément donné d'un multiensemble si celui-ci existe dans le multiensemble
                  et a le même nombre d'occurrence.
    Entrées :   l'élément à supprimer ainsi que sa multiplicité, dans le multiensemble donné a
    Sorties :   un multiensemble, résultat de la suppression
  *)
  let rec remove (x : 'a * int)(a : 'a mset) : 'a mset =
    match a with
    | [] -> []
    | t::q -> if x = t then q
      else t::(remove x q)

  (*
    Type    :   'a * int -> 'a mset -> 'a mset
    Rôle    :   Cette fonction retire un certain nombre d'occurrence d'un élément donné d'un multiensemble 
                  si celui-ci existe dans le multiensemble.
    Entrées :   l'élément à supprimer ainsi que sa multiplicité, dans le multiensemble donné a
    Sorties :   un multiensemble, résultat de la suppression
  *)
  let rec removeOcc ((v, nb) : 'a * int)(a : 'a mset) : 'a mset =
    match a with
    | [] -> []
    | (v1, nb1)::q -> if v1 = v then
	if nb1 <=  nb then q else (v1, (nb1-nb))::q
      else (v1, nb1)::(removeOcc (v, nb) q) ;;

  (*
    Type    :   'a mset -> 'a mset -> bool
    Rôle    :   Cette fonction teste l'égalité de deux multiensembles
    Entrées :   les deux multiensembles à comparer
    Sorties :   un booléen, résultat de la comparaison
  *)
  let rec eg (a : 'a mset)(b : 'a mset) : bool =
    match a with
    | [] -> (b = vide)
    | t::q -> (appart_couple t b) && (eg q (remove t b))

  (*
    Type    :   'a mset -> 'a mset -> 'a mset
    Rôle    :   Cette fonction calcul la différence de deux multiensembles
    Entrées :   deux multiensemble a et b
    Sorties :   un multiensemble, résultat de la différence de a et b (a\b)
  *)
  let rec difference (a : 'a mset)(b : 'a mset) : 'a mset =
    match b with
    | [] -> a
    | t::q -> difference (removeOcc t a) q ;;

  (*
    Type    :   'a mset -> int
    Rôle    :   Cette fonction calcul le nombre d'éléments présent dans un mutliensemble
                  en tenant compte de leur multiplicité.
    Entrées :   un mutliensemble
    Sorties :   un entier
  *)
  let rec nbElem (a : 'a mset) : int =
    match a with
    | [] -> 0
    | (v, nb)::q -> nb + nbElem q ;;

  (*
    Type    :   'a mset -> 'a mset -> int
    Rôle    :   Cette fonction calcul le nombre d'éléments différents de deux multiensembles
    Entrées :   deux multiensemble a et b
    Sorties :   un entier
  *)
  let nbDifference (a : 'a mset)(b : 'a mset) : int =
    nbElem (difference a b) ;;

end;;

  (** TESTS **)
  (*
    let l1 = [(1,3); (2,4)] ;;
    removeOcc (1,2) l1 ;;
    nbElem l1 ;;
    let l2 = [(1,5); (2,4); (3,5); (4,8)] ;;
    difference l2 l1 ;;
    nbElem l2 ;;
    eg l1 l2 ;;
    union l1 l2;;
    nbDifference l2 l1 ;;
  *)
