Projet Ocaml NOGUERON Matthieu et MATHIEU Tanguy
================================================

## Variations sur le Rami

I. Principe du Projet
-------------------------

Durant ce projet, nous devrons construire une machine dans le langage Ocaml permettant d'implémenter l'ensemble des règles des différentes variantes du Rami, et de pouvoir faire jouer une personne en lui proposant le chargement et la sauvegarde de partie.

Dans le cadre de la programmation fonctionnelle, ce projet a pour but de nous familiariser avec l'aspect modulaire du langage Ocaml. De ce fait lors de la réalisation des différents modules nous serons amené à travailler en interaction avec d'autres modules. Nous serons de plus, amené à travailler avec des notions vues en cours, comme l'utilisation d'analyseurs syntaxiques, de foncteurs, de la partie impérative d'Ocaml et enfin de l'utilisation de la lecture / écriture de donnée sur l'entrée / sortie standard.

II. Fonctionnalités
-------------------------

### Module multi-ensembles

L'ensemble des fonctionnalités demandées ont pu être implémentées et testées. Celle-ci sont fonctionnelles, et peuvent être utilisées dans d'autres modules externes.
Nous avons de plus été dans l'obligation d'implémenter de nouvelles fonctions pour réaliser certaines qui nous étaient demandées (add, remove, et appart_couple).


### Module Dictionnaire

L'ensemble des fonctionnalités demandées ont pu être implémentées et testées. Celle-ci sont fonctionnelles, et peuvent être utilisées dans d'autres modules externes.
Bien qu'on soit arrivé à finir l'implémentation de l'ensemble des fonctions de ce module, la fonction to_list nous aura tenue en haleine pendant un certain. Pour la finir, nous avons du prendre connaissance des idées d'autres élèves et s'inspirer de leur façon de faire pour arriver à notre implémentation de la fonction.
De plus, nous avons subis un petit piège assez déroutant, qui est que le dico contient que des mots avec des lettres en majuscules. Etant donné que la fonction d'ajout n'ajoutait que des mots étant en minuscules, on a du chercher un petit moment avant de trouver la source du problème.


### Module de sauvegarde et de lecture depuis un fichier

L'ensemble des fonctionnalités demandées ont pu être implémentées et testées. L'ensemble des fonctionnalitées de chargement et de sauvegarde peuvent être utilisé dans le programme.
Ainsi on peut charger une partie depuis un fichier, et sauvegarder une partie en cours.


### Module Lettres pour l'implémentation du "Rami des Lettres"

L'ensemble des fonctionnalités demandées ont pu être implémentées et testées. Celle-ci sont fonctionnelles, et peuvent être utilisées dans d'autres modules externes.
Le module peut être utilisé pour jouer.

### Module Rummikub pour l'implémentation du "Rami des Chiffres"

L'ensemble des fonction dans Rummikub fonctionne et sont fournie avec un jeu de test.

### Module Jeu

Le module jeu est entièrement implémenté et permet de jouer une partie de rami. Cependant lors de l'avant dernier tour, il semble que les points ne s'ajouten pas correctement aux scores des joueurs.

III. Utilisation
-------------------------

Pour utiliser notre programme, on pourra tout d'abord commencer par compîler, en utilisant la commande :
`make clean` suivie de la commande `make`

Pour lancer le Rami des Lettres, utiliser la commande `./ramideslettres`
Dans notre cas, nous avons décidé de charger un fichier depuis le dossier pour lancer la partie.

On pourra vérifier du bon fonctionnement du programme avec les combinaisons suivantes :

* (L F T E R A)(C O L L I E R) pour Albert
* (V N M M E I)(C O L L I E R)(M A N G E * A) pour Damien

Remarques :
-----------

Dans notre code, nous n'avons incorporé que l'utilisation du Rami des Lettres. Ceci est du à une erreur dans Rummikub, cependant en appliquant le foncteur Jeu sur une version valide de Rummikub, on pourrait y jouer.

