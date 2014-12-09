type dico = Noeud of dico array * bool | Feuille

let dico_vide = Noeud((Array.make 26 Feuille), false)

let mapLettre c = int_of_char c - int_of_char 'a' ;;

let x = "abc";;
(**mapLettre x.[2];;
String.length x;;
String.sub x 1 ((String.length x)-1);;**)

let rec member (s : string)(d : dico) : bool =
  match d with
  | Feuille -> false
  | Noeud(a, b) -> if String.length s = 0 then b
    else if s.[0] = '*' then
      Array.fold_right (fun x y -> (member (String.sub s 1 ((String.length s)-1)) x) || y) a false
    else
      member (String.sub s 1 ((String.length s)-1)) a.(mapLettre s.[0]) ;;

(** member "***" dico_vide ;; **)

let rec add (s : string)(d : dico) : dico =
  match d with
  | Feuille -> add s (Noeud((Array.make 26 Feuille), false))
  | Noeud(a, b) -> if String.length s = 0 then Noeud(a, true)
    else 
      let pl = mapLettre s.[0] in
      Array.set a pl (add (String.sub s 1 ((String.length s)-1)) a.(pl));
      Noeud(a, b) ;;

let rec remove (s : string)(d : dico) : dico =
  match d with
  | Feuille -> failwith "Erreur : feuille atteinte sans reconnaissance du mot."
  | Noeud(a, b) -> if String.length s = 0 then Noeud(a, false)
    else 
      let pl = mapLettre s.[0] in
      Array.set a pl (remove (String.sub s 1 ((String.length s)-1)) a.(pl));
      Noeud(a, b) ;;

(** remove "" dico_vide ;;
remove "abcde" dico_vide ;;
dico_vide;;
add x dico_vide ;;
remove x dico_vide ;;
member "" dico_vide;;
member x dico_vide ;; **)

(**
let of_stream (cs : char Stream.t) : dico =;;
let to_list(d : dico) : string list =;;**)
