module MultiEnsemble =
struct

  type 'a mset = ('a * int) list

  let vide = []

  let rec add ((v, nb) : 'a * int)(a : 'a mset) : 'a mset =
    match a with
    | [] -> [(v, nb)]
    | t::q -> let (v1, nb1) = t in if v = v1 then (v, (nb+nb1))::q
      else t::(add (v, nb) q)

  let rec union (a : 'a mset)(b : 'a mset) : 'a mset = 
    match a with 
    | [] -> b
    | t::q -> union q (add t b)
      
  let rec appart (x : 'a)(a : 'a mset) : bool =
    match a with
    | [] -> false
    |t::q -> let (v, nb) = t in (x = v) || (appart x q)

  let rec appart_couple (x : 'a * int)(a : 'a mset) :  bool =
    match a with
    | [] -> false
    | t::q -> (x = t) || (appart_couple x q)

  let rec remove (x : 'a * int)(a : 'a mset) =
    match a with
    | [] -> []
    | t::q -> if x = t then q
      else t::(remove x q)

  let rec eg (a : 'a mset)(b : 'a mset) : bool =
    match a with
    | [] -> (b = vide)
    | t::q -> (appart_couple t b) && (eg q (remove t b))

(** let l1 = [(1,3); (2,4)] ;;
    let l2 = [(1,3); (2,4); (3,5); (4,8)] ;;
    eg l1 l2 ;;

    union l1 l2;; **)
end;;
