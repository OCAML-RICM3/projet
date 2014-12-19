(*#use "regle.mli"
#use "MultiEnsemble.ml"*)

(*type token = LPar | RPar | TGen of string*)

open MultiEnsemble
open Regle

module Rummikub =
struct
  
  type couleur = Bleu | Rouge | Jaune | Noir
  type t = Tuile of (int*couleur) | Joker  
  type combi = t list
  type main = t MultiEnsemble.mset

  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int} ;;
       
  let rec auxpaquet nb l = match nb with
	    |0 -> l
	    |i -> let l' = (Tuile(i,Bleu),2)::(Tuile(i,Rouge),2)::(Tuile(i,Jaune),2)::(Tuile(i,Noir),2)::l
		  in auxpaquet (i-1) l';;

  let paquet =  (Joker,2)::(auxpaquet 13 []);;

  (*  suite_valide_aux
    Type    :   t -> int -> combi -> bool
    Rôle    :   Retourne vrai si t::combi est une suite valide
    Entrées :   t la premier tuile, int le nonbre de tuile de la rencontrée (les tuile dans combi ne sont pas encore comté)
    Sorties :   booleen
  *)
  let rec suite_valide_aux (tPrec:t)  (occ:int) (combin:combi):bool =
			match combin with
			|[] -> occ >= 3
			|Joker::l -> let Tuile(e,c)=tPrec in suite_valide_aux (Tuile(e+1,c)) (occ+1) l
			|(Tuile(ent,coul))::l -> let Tuile(e,c)=tPrec 
						 in if ent = e + 1 && coul = c then
						 	suite_valide_aux (Tuile(ent,coul)) (occ+1)  l
						   else false;;

(*  suite_valide
    Type    :   combi -> bool
    Rôle    :   Retourne vrai si combi est une suite valide
    Entrées :   t la premier tuile, int le nonbre de tuile de la rencontrée (les tuile dans combi ne sont pas encore comté)et combi une combi
    Sorties :   booleen
  *)
  let suite_valide combin = let Tuile(ent,coul)::l=combin in suite_valide_aux (Tuile(ent,coul)) 1 l;;
            	
(*  appartien
    Type    :   'a -> 'a list -> bool
    Rôle    :   Retourne vrai si 'a apartien a la 'a list
    Entrées :   elt et list elt
    Sorties :   booleen
  *)	
  let rec appartien a l = match l with
    |[]-> false
    |t::q -> a=t || (appartien a q);;

(*  groupe_valide_aux
    Type    :   occ:int -> nb:int -> couleur list -> combi -> bool
    Rôle    :   Retourne vrai si combi+ (la tuile representé par nb et la seull couleur de la liste) est un groupe valide
    Entrées :   occ le nombre de tuille de la combinaison totale, nb la valeur de toute les tuile de la combinaison, 
                couleur liste la liste des couleur rencontré et combi la suite de la combi a testé
    Sorties :   booleen
  *)
  let rec groupe_valide_aux occ nb (lcouleur: couleur list) (combinaison:combi) = 
				match combinaison with
				|[] -> occ = 3 || occ = 4
				|Joker::l -> groupe_valide_aux (occ+1) nb lcouleur l				
				|(Tuile(ent,coul))::l -> if (not(appartien coul lcouleur) && nb = ent) then
								groupe_valide_aux (occ+1) nb (coul::lcouleur) l
				                         else false;;
  

(*  groupe_valide
    Type    :   combi -> bool
    Rôle    :   Retourne vrai si combi est un groupe valide
    Entrées :   combin une combi
    Sorties :   booleen
  *)
  let groupe_valide (combin:combi):bool = let Tuile(ent,coul)::l=combin in groupe_valide_aux 1 ent [coul] l;;


(*  combi_valide_aux
    Type    :   combi -> bool
    Rôle    :   Retourne vrai si combi est une suite valide ou un groupe valide ou vide
    Entrées :   combi une combi
    Sorties :   booleen
  *)
  let combi_valide (combi:combi):bool = []=combi|| (groupe_valide combi )||(suite_valide combi);; 
	
(*  nb_element
    Type    :   a' list-> int
    Rôle    :   Retourne le nombre d'element de la liste
    Entrées :   une liste de n'importequoi
    Sorties :   int
  *)	
  let rec nb_element l = match l with
   |[]-> 0
   |t::q -> 1+ (nb_element q);;

(*  point_combi
    Type    :   combi -> int
    Rôle    :   Retourne le nombre de point fait par la combinaison
    Entrées :   une combi
    Sorties :   un int
  *)
  let points_combi (combin:combi)  = if groupe_valide combin then
      match combin with
        |Tuile(n,_)::_ -> n*(nb_element combin)
        |Joker::Tuile(n,_)::_ -> n*(nb_element combin)
        |Joker::Joker::Tuile(n,_)::_ -> n*(nb_element combin)
        else if suite_valide combin then match combin with
        |Tuile(n,_)::l -> (2*n+(nb_element l))*(nb_element combin)/2
        |Joker::Tuile(n,_)::l -> (2*n-2+((nb_element l)+1))*(nb_element combin)/2
        |Joker::Joker::Tuile(n,_)::l ->  (2*n-4+((nb_element l)+2))*(nb_element combin)/2
        else failwith "combinaison non valide";; 
		 
(*  premier_coup_valide_aux
    Type    :   int -> combi list -> bool
    Rôle    :   renvoi vrai si le le nombr de point fait par les combinaison est superieur a 30
    Entrées :   int le compteur de point, combi list la list des autre combi
    Sorties :   booleen
  *)
  let rec premier_coup_valide_aux nb listeCombi = match listeCombi with
  |[]-> nb>= 30
  |t::q -> (combi_valide t)&& (premier_coup_valide_aux (nb+ points_combi t) q);;

		
(*  premier_coup_valide
    Type    :   main -> combi list -> main -> bool
    Rôle    :   Retourne vrai si la combi list a plus de 30 point
    Entrées :   mainInit la main avant les combi, combi list la list des combinaison a faire, mainFinale la main aprés avoir fait les combi
    Sorties :   booleen
  *)		
  let premier_coup_valide (mainInit:main) (listeCombi:combi list) (mainFinale:main):bool = premier_coup_valide_aux 0 listeCombi;;			
		
	
(*  suite_valide_aux
    Type    :   t -> int -> combi -> bool
    Rôle    :   Retourne vrai si t::combi est une suite valide
    Entrées :   t la premier tuile, int le nonbre de tuile de la rencontrée (les tuile dans combi ne sont pas encore comté)
    Sorties :   booleen
  *)
  let points listeCombiActuel (mainInitiale:main) (listeCombiFinale:combi list) (mainFinale:main) = 0;;

(*  suite_valide_aux
    Type    :   t -> int -> combi -> bool
    Rôle    :   Retourne vrai si t::combi est une suite valide
    Entrées :   t la premier tuile, int le nonbre de tuile de la rencontrée (les tuile dans combi ne sont pas encore comté)
    Sorties :   booleen
  *)
  let rec points_finaux_aux point main:int= match main with
    |[]-> point
    |(Joker,occ)::l -> points_finaux_aux (point + occ*30) l
    |(Tuile(nb,_),occ)::l -> points_finaux_aux (point + nb*occ) l;;

  
  let main_initiale = 14;;
	
  let main_min = 0;;

	(*  suite_valide_aux
    Type    :   t -> int -> combi -> bool
    Rôle    :   Retourne vrai si t::combi est une suite valide
    Entrées :   t la premier tuile, int le nonbre de tuile de la rencontrée (les tuile dans combi ne sont pas encore comté)
    Sorties :   booleen
  *)
  let lit_valeur listetoken =  match listetoken with
    |[TGen((tuile:string))] -> Joker
    |[LPar;TGen(coul);TGen(ent);RPar] -> let numero = int_of_string ent in
					      	       if coul.[0] = 'b' then
					       		Tuile(numero,Bleu)
					       	       else if coul.[0] = 'r' then
					       		Tuile(numero,Rouge)
					       	       else if coul.[0] = 'j' then
					       		Tuile(numero,Jaune)
					       	       else 
					       		Tuile(numero,Noir)					
    |_ -> failwith"il n'y a pas de valeur";; 
 

(*  suite_valide_aux
    Type    :   t -> int -> combi -> bool
    Rôle    :   Retourne vrai si t::combi est une suite valide
    Entrées :   t la premier tuile, int le nonbre de tuile de la rencontrée (les tuile dans combi ne sont pas encore comté)
    Sorties :   booleen
  *)
	let ecrit_valeur tuile = match tuile with
					|Joker -> "joker"
					|Tuile(ent,coul) -> let nb = ((string_of_int ent)^(")")) in 
							     if coul = Rouge then
								("(rouge " ^ nb)
							     else if coul = Jaune then
								("(jaune " ^ nb)
							     else if coul = Bleu then
								("(bleu " ^ nb)
							     else
								("(noir " ^ nb);;
					

		
	

	let fin_pioche_vide = true;;
	
  
end;;

(*
let comb1 =[Tuile(6, Bleu); Tuile (6, Rouge); Tuile (6, Jaune);Joker];;
combi_valide comb1;;

let comb2 =[Tuile (7, Bleu); Joker;Tuile (9, Bleu); Tuile (10, Bleu)];;


combi_valide comb2;;

let lcombi = [comb1;comb2];;

premier_coup_valide (a:main) lcombi (b:main);;
*)
