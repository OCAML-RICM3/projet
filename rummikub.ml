#use "Dictionnaire.ml"

module Rummikub : REGLES =
struct

  type couleur = Bleu | Rouge | Jaune | Noir
  type t = Tuile of (int*couleur) | Joker  type t = char
  type combi = t list
  type main = t MultiEnsemble.mset

  type etat = { noms: string array; scores: int array; mains: main array;
		table: combi list; pioche: main; pose: bool array; tour: int} ;;

  let paquet = [ ('A', 8); ('B', 2); ('C', 3); ('D', 3); ('E', 16); ('F', 2); ('G', 2);
 ('H', 2); ('I', 9); ('J', 1); ('K', 1); ('L', 6); ('M', 4); ('N', 7); ('O', 7);
 ('P', 2); ('Q', 1); ('R', 7); ('S', 7); ('T', 7); ('U', 7); ('V', 2); ('W', 1);
 ('X', 1); ('Y', 1); ('Z', 1)];;let liste = ref [] in 
		     liste := [(Joker,2)]@(!liste);
		     for i = 13 downto 1 do
			liste := [(Tuile(i,Bleu),2);(Tuile(i,Rouge),2);(Tuile(i,Jaune),2);(Tuile(i,Noir),2)]@(!liste);
		     done;
		     !liste

	(*
	Nom: combi_valide
	Type: combi -> bool
	Description: Elle verifie si la combinaison posee est valide a travers deux fonctions dans le cas ou la combinaison contient des tuiles successives ou alors le cas
	ou les tuiles ont le meme numero mais pas la meme couleur.
	Entree:
		- combinaison: liste de tuiles
	Sortie:
		- booleen: vrai si la combi est valide, faux sinon
	*)
		
	let combi_valide combinaison =
		let longueur = List.length combinaison in		
		
		(* La fonction tuilesSuccessives prend en entrée un numero, une couleur et une combinaison et renvoie un numero (int).
		Elle permet de verifier si, dans une combinaison, des tuiles se suivent.*)

		let rec tuilesSuccessives numero couleur combinaison =
			match combinaison with
			|[] -> numero
			|Joker::l -> tuilesSuccessives (numero + 1) couleur l
			|(Tuile(ent,coul))::l -> if ent = numero + 1 && coul = couleur then
						 	tuilesSuccessives ent coul l
						 else
							raise TuilesPasSuccessives	
		in
		
		(* La fonction groupeTuiles prend en entrée une combinaison et renvoie un booleen. Elle permet de verifier si, dans
		une combi, des tuiles ont le meme numero mais pas la meme couleur.*)

		let rec groupeTuiles combinaison =
			let rec compare numero couleur combinaison = 
				match combinaison with
				|[] -> true
				|Joker::l -> compare numero couleur l				
				|(Tuile(ent,coul))::l -> (if (couleur != coul && numero = ent) then
								compare numero couleur l
							  else
								false)
			in
			
			match combinaison with
				|[] -> true
				|Joker::l -> groupeTuiles l
				|(Tuile(ent,coul))::l -> (if (compare ent coul l) then
								groupeTuiles l
							  else
								false)
		in
		
		(* combi_valideIN prend en entrée la longueur de la combinaison et la combinaison. En utilisant groupeTuiles et tuilesSuccessives, elle nous permet
		de renvoyer un booleen.*)
		
		let rec combi_valideIN longueur combinaison =
			match combinaison with
				|[] -> true
				|Joker::l -> combi_valideIN longueur l
				|(Tuile(ent,coul))::l -> try(
								 if ((groupeTuiles combinaison) && (longueur >= 3) && (longueur <= 4)) then
									true
								 else 
									(((tuilesSuccessives ent coul l)- ent) >= 2)
							
						 	    ) with 
								| TuilesPasSuccessives -> false	
						    

		in
		combi_valideIN longueur combinaison


	(*--------------------------------------------*)

	
	(*
	Nom: premier_coup_valide
	Type: main -> combi list -> main -> bool
	Description: La fonction premier_coup_valide verifie si le premier coup d'un joueur est valide. 
	Entree:
		- mainInitiale: main du joueur
		- listeCombi: pose du joueur
		- mainFinale: nouvelle main du joueur
	Sortie:
		- booleen: vrai si toutes les combinaisons sont valides et si la somme des entiers des cartes vaut au moins 30
	*)

	let premier_coup_valide mainInitiale listeCombi mainFinale =
	(	
		(*la fonction combinaisonValide utilise la fonction combi_valide ecrite precedemment mais l'applique sur une liste de combinaisons et 
		non pas une seule combinaison*)

		let rec combinaisonValide listeCombi =		
			match listeCombi with
				|[]-> true
				|x::l -> (if combi_valide x then
						combinaisonValide l
					  else
						false)
		in
		
		(*La fonction creationMainInitiale prenant en entree une liste de combinaisons et la main finale est la meme fonction que celle
		utilisee dans le module Lettres sauf que les parametres sont differents. Elle renvoie un booleen. *)
		
		let rec creationMainInitiale listeCombi mainFinale =
			(
				let ajouteTuile main tuile = match tuile with
								|Joker -> MultiEnsemble.union ([(Joker,1)]) main 			
								|Tuile(ent,coul) -> MultiEnsemble.union ([(Tuile(ent,coul),1)]) main
				in
	
				let rec ajouteCombi main combi = 
					match combi with
					| []-> main
					| x::l -> ajouteCombi (ajouteTuile main x) l
				in
				
				(			
					match listeCombi with
						| [] -> MultiEnsemble.egalite mainInitiale mainFinale
						| x::l -> creationMainInitiale l (ajouteCombi mainFinale x)
				)			
		
			)
		in
		
		(* La fonction pointsTuiles, comme son nom l'indique, permet de calculer le nombre de points a partir des tuiles qui ont ete
		posees (hors joker). Elle prend en parametres une liste et un int et renvoie un int. *)

		let rec pointsTuiles liste points =
			match liste with
				|[] -> points
				|Joker::l -> pointsTuiles l points
				|Tuile(ent,_)::l -> pointsTuiles l (points + ent)
		in
		
		(* La fonction pointsJoker permet de calculer les points provenant des cartes Joker. Elle prend en entree une liste et renvoie un int.
		Dans les cas où le Joker peut prendre la valeur de Tuiles différentes, nous avons retenu le plus haut score, notamment dans les combinaisons
		ou il y a deux jokers. *)
		
		let pointsJoker liste=
			match liste with
				|[Tuile(ent1,_);Tuile(ent2,_);Joker] -> if (ent1 = ent2) then ent1 else (ent2+1)																 
				|[Tuile(ent1,_);Joker;Tuile(ent2,_)] -> if (ent1 = ent2) then ent1 else (ent1+1)
				|[Joker;Tuile(ent1,_);Tuile(ent2,_)] -> if (ent1 = ent2) then ent1 else (ent1-1)
				|[Tuile(ent,_);Joker;Joker] -> ((ent+1)+(ent+2)) 
				|[Joker;Tuile(ent,_);Joker] -> (2*ent)
				|[Joker;Joker;Tuile(ent,_)] -> (2*ent)
				|[Tuile(ent1,_);Tuile(ent2,_);Tuile(ent3,_);Joker] -> if (ent1 = ent2) then ent1 else (ent3+1)
				|[Tuile(ent1,_);Tuile(ent2,_);Joker;Tuile(ent3,_)] -> if (ent1 = ent2) then ent1 else (ent2+1)
				|[Tuile(ent1,_);Joker;Tuile(ent2,_);Tuile(ent3,_)] -> if (ent1 = ent2) then ent1 else (ent1+1)
				|[Joker;Tuile(ent1,_);Tuile(ent2,_);Tuile(ent3,_)] -> if (ent1 = ent2) then ent1 else (ent1-1)				
				|[Tuile(ent1,_);Tuile(ent2,_);Joker;Joker] -> if (ent1 = ent2) then (2*ent1) else (ent2+1+ent2+2)
				|[Tuile(ent1,_);Joker;Tuile(ent2,_);Joker] -> if (ent1 = ent2) then (2*ent1) else (ent1+1+ent2+2)
				|[Joker;Tuile(ent1,_);Tuile(ent2,_);Joker] -> if (ent1 = ent2) then (2*ent1) else (ent1+1+ent2+2)
				|[Joker;Tuile(ent1,_);Joker;Tuile(ent2,_)] -> (2*ent1)
				|[Joker;Joker;Tuile(ent1,_);Tuile(ent2,_)] -> if (ent1 = ent2) then (2*ent1) else (ent1-1+ent1-2)
				|_ -> 0		
		in
		
		(*La fonction points utilise les fonctions pointsTuiles et pointsJoker pour calculer le nombre total de points. Elle prend en entrée
		une liste de combinaisons, les points des Tuiles et les points des Joker. Elle renvoie un int (le total des points).*)	
	
		let rec points listeCombi tPoints jPoints = 
			match listeCombi with
				|[] -> (tPoints + jPoints)
				|x::l -> points l (pointsTuiles x tPoints) (pointsJoker x)
		in

		
		match listeCombi with
			|[] -> false
			|x::l -> if (((points listeCombi 0 0)>=30) && (combinaisonValide listeCombi) && (creationMainInitiale listeCombi mainFinale)) then
					true
				 else
					false
	)

	
	(*--------------------------------------------*)


	(*
	Nom: points
	Type: combi list -> main -> combi list -> main -> int
	Description: Aucune utilite dans ce module
	Entree:
		- listeCombiActuel: jeu en cours 
		- mainInitiale: main initiale du joueur
		- listeCombiFinale: nouveau jeu 
		- mainFinale: main finale du joueur
	Sortie:
		- entier: 0
	*)

	let points listeCombiActuel mainInitiale listeCombiFinale mainFinale = 0


	(*--------------------------------------------*)


	(*
	Nom: points_finaux
	Type: main -> int
	Description: elle permet de calculer le nombre de points de la main d'un joueur lorsque la partie est terminee.
	Entree:
		- main: mset contenant les dernieres tuiles d'un joueur lorsque la partie est terminee
	Sortie:
		- entier: somme des points des differentes tuiles
	*)
	
	let points_finaux main = 
		
		let rec points_finauxIN points main =		
			match main with
				| [] -> points
				|(Joker,ent)::l -> points_finauxIN (points + ent*30) l
				|(Tuile(ent,_),ent2)::l -> points_finauxIN (points + ent*ent2) l
		in
		
		points_finauxIN 0 main
	
	
	(*--------------------------------------------*)
	

	(* Les fonctions lit_valeur et ecrit_valeur sont utilisees pour la sauvegarde et le chargement d'une partie*)

	(*
	Nom: lit_valeur
	Type: token list -> t
	Description: fonction utilisee dans ParserRami.ml (Sauvegarde/Lecture d'un fichier)
	Entree:
		- listetoken: liste de tokens
	Sortie:
		- caractere: Tuile ou Joker
	*)


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


	(*
	Nom: ecrit_valeur
	Type: t -> string
	Description: fonction utilisee dans ParserRami.ml (Sauvegarde/Lecture d'un fichier)
	Entree:
		- tuile: Joker ou Tuile
	Sortie:
		- chaine: chaine creee a partir de la tuile (elle va etre de la forme "(couleur entier)")
	*)	

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
					

	(*--------------------------------------------*)

		
	let main_initiale = 14
	
	let main_min = 0

	let fin_pioche_vide = false
 
  
end;;
