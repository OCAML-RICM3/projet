#load "dynlink.cma"
#load "camlp4o.cma"

module Dictionnaire =
struct

  type dico = Noeud of dico array * bool | Feuille

  let dico_vide = Noeud((Array.make 26 Feuille), false)

  (** Transforme une lettre en un entier équivalent à sa position dans l'alphabet
      de ce fait, 'a' -> 0 et 'z' -> 25 **)
  let mapLettre c = int_of_char c - int_of_char 'a' ;;

  (**let x = "abc";;
  mapLettre x.[2];;
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

  (** Parser utiliser pour récuperer un mot dans un stream **)
  let rec mot s = parser
    | [< ''a'..'z' as c ; m = mot (s^(String.make 1 c)) >] -> m 
    | [< ''\n' >] -> s;;

  (* let str = Stream.of_string "ab\nabc\naaa\nabd\nchat\n";; *)
  (* mot "" str;; *)

  let rec of_stream = parser
    | [< m = mot "" ; em = of_stream >] -> add m em
    | [< >] -> dico_vide ;;

(*   of_stream str ;; *)

  (** Transforme un nombre en la lettre associé, selon la position dans l'alphabet
      de ce fait, 0 -> 'a' et 25 -> 'z' **)
  let nbToLetter(x : int) : string = 
    let c = char_of_int (int_of_char 'a' + x) in String.make 1 c ;;
  
  (* nbToLetter 0 ;; 
  nbToLetter 25 ;; *)

  let to_list (d : dico) : string list = 
    let rec to_listSub (d1 : dico)(s : string) =
      match d with
      | Feuille -> []
      | Noeud(a, b) -> let list = ref [] in
		       for i=0 to 25 do
			 list := (!list)@(to_listSub a.(i) (s^(nbToLetter i)));
		       done;
		       if b then 
			 s :: (!list)
		       else 
			 !list
    in to_listSub d "";;
    
(**  to_list dico_vide;; **)

end;;
