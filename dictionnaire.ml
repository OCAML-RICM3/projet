type dico = Noeud of dico array * bool | Feuille

let dico_vide = Noeud((Array.make 26 Feuille), false)

let mapLettre c = int_of_char c - int_of_char 'a' ;;

let x = "aed";;
mapLettre x.[2];;
String.length x;;
String.sub x 1 ((String.length x)-1);;

let rec member (s : string)(d : dico) : bool =
  match d with
  | Feuille -> false
  | Noeud(a, b) -> if String.length s = 0 then b
    else member (String.sub s 1 ((String.length s)-1)) a.(mapLettre s.[0]) ;;

let rec add (s : string)(d : dico) : dico =
  match d with
  | Feuille -> add s (Noeud((Array.make 26 Feuille), false))
  | Noeud(a, b) -> if String.length s = 0 then Noeud(a, true)
    else 
      let pl = mapLettre s.[0] in
      Array.set a pl (add (String.sub s 1 ((String.length s)-1)) a.(pl));
      Noeud(a, b) ;;

add x dico_vide ;;

(**
let remove (s : string)(d : dico) : dico =;;
let of_stream (cs : char Stream.t) : dico =;;
let to_list(d : dico) : string list =;;**)
