(**
#load "dynlink.cma"
#load "camlp4o.cma"
**)

module Dictionnaire =
struct

  type dico = Noeud of dico array * bool | Feuille

  let dico_vide() = Noeud((Array.make 26 Feuille), false)

  (*
    Type    :   char -> int
    Rôle    :   Transforme une lettre en un entier équivalent à sa position dans l'alphabet.
                  De ce fait, 'a' -> 0 et 'z' -> 25.
    Entrées :   un caractère
    Sorties :   un entier
  *)
  let mapLettre c = int_of_char c - int_of_char 'A' ;;

  (*
    Type    :   string -> dico -> bool
    Rôle    :   Recherche une chaine de caractères dans un dictionnaire passé en argument. De plus, la fonction
                  tiendra compte de l'utilisation de jokers.
    Entrées :   une chaine, un dictionnaire
    Sorties :   un booléen, résultat de la recherche
  *)
  let rec member (s : string)(d : dico) : bool =
    match d with
    | Feuille -> false
    | Noeud(a, b) -> if String.length s = 0 then b
      else if s.[0] = '*' then
	Array.fold_right (fun x y -> (member (String.sub s 1 ((String.length s)-1)) x) || y) a false
      else
	member (String.sub s 1 ((String.length s)-1)) a.(mapLettre s.[0]) ;;

  (*
    Type    :   string -> dico -> dico
    Rôle    :   Ajoute un mot dans un dictionnaire
    Entrées :   une chaine, un dictionnaire
    Sorties :   le dictionnaire, résultat de l'ajout de la chaine au dictionnaire initial
  *)
  let rec add (s : string)(d : dico) : dico =
    match d with
    | Feuille -> add s (dico_vide())
    | Noeud(a, b) -> if String.length s = 0 then Noeud(a, true)
      else 
	let pl = mapLettre s.[0] in
	  a.(pl) <- (add (String.sub s 1 ((String.length s)-1)) a.(pl));
	  Noeud(a, b) ;;

  (*
    Type    :   string -> dico -> dico
    Rôle    :   Supprimer un mot dans un dictionnaire (ne supprime pas les branches inutiles)
    Entrées :   une chaine, un dictionnaire
    Sorties :   le dictionnaire, résultat de la suppression de la chaine au dictionnaire initial
  *)
  let rec remove (s : string)(d : dico) : dico =
    match d with
    | Feuille -> failwith "Erreur : feuille atteinte sans reconnaissance du mot."
    | Noeud(a, b) -> if String.length s = 0 then Noeud(a, false)
      else 
	let pl = mapLettre s.[0] in
	Array.set a pl (remove (String.sub s 1 ((String.length s)-1)) a.(pl));
	Noeud(a, b) ;;

  (*
    Type    :   char Stream.t -> string
    Rôle    :   Parse un stream de char sous la forme d'une chaine de caractères.
                  Si on arrive au caractère '\n' on renvoi la chaine vide.
    Entrées :   un stream de char à parser
    Sorties :   une chaine, résultat du parsing
  *)
  let rec mot= parser
    | [< ''a'..'z' as c; s >] -> (String.make 1 c) ^ (mot s) 
    | [< ''\n' >] -> "";;

  (*
    Type    :   char Stream.t -> dico
    Rôle    :   Lis un stream de caractère et le transforme en dictionnaire
    Entrées :   un stream de char à parser
    Sorties :   un dictionnaire, résultat du parsing
  *)
  let rec of_stream = parser
    | [< m = mot; em = of_stream >] -> add m em
    | [< >] -> dico_vide() ;;

  (*
    Type    :   int -> string
    Rôle    :   Transforme un nombre en la lettre associé (sous forme de string), selon la position 
                  dans l'alphabet. De ce fait, 0 -> "a" et 25 -> "z" 
    Entrées :   un entier
    Sorties :   une chaine
  *)
  let nbToLetter(x : int) : string = 
    let c = char_of_int (int_of_char 'a' + x) in String.make 1 c ;;

  (*
    Type    :   dico -> string list
    Rôle    :   Convertit un dico en une liste de string représentant l'ensemble des mots
                  présents dans le dictionnaire.
    Entrées :   le dictionnaire à convertir
    Sorties :   une liste de chaine de caractères, résultat de la conversion
  *)
  let to_list (d : dico) : string list = 
    let rec to_listSub (d1 : dico)(s : string) =
      match d1 with
      | Feuille -> []
      | Noeud(a, b) -> let listRef = ref [] in
		       for i=0 to 25 do 
			 listRef := (!listRef)@(to_listSub a.(i) (s^(nbToLetter i)));
		       done;
		       if b then
			 s :: (!listRef)
		       else 
			 !listRef
    in to_listSub d "" ;;

end;;


(** TESTS **)

(*
  let x = "abc" ;;
  mapLettre x.[2] ;;
  let dico = add x (dico_vide());;
  add "aaa" dico ;;
  to_list dico ;;

  member "***" dico_vide ;;

  remove "" dico_vide ;;
  remove "abcde" dico_vide ;;
  dico_vide;;
  add x dico_vide ;;
  remove x dico_vide ;;
  member "" dico_vide;;
  member x dico_vide ;;

let valide s =
  ((String.length s) <> 0) &&
    begin
      let ret = ref true in
      for i = 0 to (String.length s) - 1 do
	let c = Char.code s.[i] in
	ret := (!ret) && (c >= (Char.code 'A')) && (c <= (Char.code 'Z'))
      done;
      !ret
    end;;

(* Permet de charger un dictionnaire en mettant tous les mots en majuscules *)
(* Les mots avec accents sont supprimés. *)
(* La fonction add du dictionnaire doit avoir été déjà implantée. *)

let rec affiche (l : string list) = 
  match l with
  | [] -> ""
  | t::q -> t^"\n"^(affiche q);;

let dico =
  let flux = open_in "test.txt" in
  let mondico = ref (dico_vide()) in
  try
    begin
      while true do
	let l = String.uppercase (input_line flux) in
	if (valide l) then
	  mondico := add l (!mondico);
      done;
      !mondico
    end
  with
    End_of_file -> !mondico;;

  let str = Stream.of_string "ab\nabc\naaa\nabd\nchat\n" ;;
  mot str;;

  print_endline(affiche(to_list(of_stream str))) ; ;;

  to_list dico ;;

  nbToLetter 0 ;; 
  nbToLetter 25 ;;
  to_list dico ;; *)
