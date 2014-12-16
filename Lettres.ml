type token = LPar | RPar | TGen of string;;

module Lettres : REGLES =
struct

  type t = char
  type combi = t list
  type main = t MultiEnsemble.mset

  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int} ;;

  let paquet = [ ('A', 8); ('B', 2); ('C', 3); ('D', 3); ('E', 16); ('F', 2); ('G', 2);
 ('H', 2); ('I', 9); ('J', 1); ('K', 1); ('L', 6); ('M', 4); ('N', 7); ('O', 7);
 ('P', 2); ('Q', 1); ('R', 7); ('S', 7); ('T', 7); ('U', 7); ('V', 2); ('W', 1);
 ('X', 1); ('Y', 1); ('Z', 1)];;

  let rec charListToString (c : char list) =
    match c with
    | [] -> String.make 0 'a'
    | t::q -> (String.make 1 t)^charListToString q;;

(* Permet de déterminer si une chaîne de caractères est un mot en majuscules *)
  let valide s =
    ((String.length s) <> 0) &&
      begin
	let ret = ref true in
	for i = 0 to (String.length s) - 1 do
	  let c = Char.code s.[i] in
	  ret := (!ret) && (c >= (Char.code 'A')) && (c <= (Char.code 'Z'))
	done;
	!ret
      end

  let combi_valide (c : combi) : bool = 
   let s = charListToString c in 
   (Dictionnaire.member s Dictionnaire.dico_vide) 
   && (String.length s >= 3) && (valide s) ;; 
  

 (** let premier_coup_valide (m : main (* main du joueur *))
      (p : combi list (* pose du joueur *))
      (n : main (* nouvelle main du joueur *)) : bool =

    let points (c : combi list (* jeu en cours *))(m : main (* main du joueur *))
	(n : combi list (* nouveau jeu *))
	(nm : main (* nouvelle main du joueur *)) : int =

      let points_finaux (m : main) : int = **)

  let main_min = 7;;
  let main_initiale = 14;;

(*  let lit_valeur (t : token list) : t = 

  let ecrit_valeur (t : t) : string = *)

  let fin_pioche_vide = true

end;;
