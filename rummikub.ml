#use "regle.mli"

type token = LPar | RPar | TGen of string
module Rummikub : REGLES =
struct
  
  type cocleur = Bleu | Rouge | Jaune | Noir
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

 
  let rec suite_valide_aux (tPrec:t)  (occ:int) (combin:combi):bool =
			match combin with
			|[] -> occ >= 3
			|Joker::l -> let Tuile(e,c)=tPrec in suite_valide (Tuile(e+1,c)) (occ+1) l
			|(Tuile(ent,coul))::l -> let Tuile(e,c)=tPrec 
						 in if ent = e + 1 && coul = c then
						 	suite_valide (Tuile(ent,coul)) (occ+1)  l
						   else false;;

  let suite_valide combin = let Tuile(ent,coul)::l=combin in suite_valide_aux (Tuile(ent,coul)) 1 l
            		
  let rec appartien a l = match l with
    |[]-> false
    |t::q -> a=t || (appartien a q);;

  let rec groupe_valide_aux occ nb lcouleur combinaison = 
				match combinaison with
				|[] -> occ = 3 || occ = 4
				|Joker::l -> groupe_valide (occ+1) nb lcouleur l				
				|(Tuile(ent,coul))::l -> if (not(appartien coul lcouleur) && nb = ent) then
								groupe_valide (occ+1) nb (coul::lcouleur) l
				                         else false;;
  let groupe_valide combin = let Tuile(ent,coul)::l=combin in groupe_valide_aux 1 ent [coul] l;;

  let combi_valide combi = []=combi|| (groupe_valide )||(suite_valide combi;; 
	
	
  let rec nb_element l = match l with
   |[]-> 0
   |t::q -> 1+ (nb_element q);;

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
		 
let premier_coup_valide mainInit listeCombi mainFinale = 

				
			
		
	





	let points_finaux main = ;;
		
	
	let lit_valeur listetoken = 
		match listetoken with
			| [TGen((tuile:string))] -> Joker
			| [LPar;TGen(coul);TGen(ent);RPar] -> (let numero = int_of_string ent in
							       if coul.[0] = 'b' then
								Tuile(numero,Bleu)
							       else if coul.[0] = 'r' then
								Tuile(numero,Rouge)
					   		       else if coul.[0] = 'j' then
								Tuile(numero,Jaune)
					  		       else if coul.[0] = 'n' then
								Tuile(numero,Noir)					
							       else
								raise ProblemeParser)
			| _ -> raise ProblemeParser



	let ecrit_valeur tuile = match tuile with
					|Joker -> "joker"
					|Tuile(ent,coul) -> (let ch = ((string_of_int ent)^(")")) in 
							     if coul = Bleu then
								("(bleu " ^ ch)
							     else if coul = Rouge then
								("(rouge " ^ ch)
							     else if coul = Jaune then
								("(jaune " ^ ch)
							     else if coul = Noir then
								("(noir " ^ ch)					
							     else
								raise ProblemeParser)
					

		
	let main_initiale = 14
	
	let main_min = 0

	let fin_pioche_vide = false
	
  
end;;
