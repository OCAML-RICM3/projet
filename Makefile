# BUILD RULES
all: 
	ocamlc -c MultiEnsemble.mli
	ocamlc -c MultiEnsemble.ml
	
	ocamlc -c regle.mli
	
	ocamlc -c Dictionnaire.mli
	ocamlc -c -pp camlp4o Dictionnaire.ml
	
	ocamlc -c -pp camlp4o Lettres.ml
	
	ocamlc -c LoadSaveRami.mli
	ocamlc -c -pp camlp4o LoadSaveRami.ml
	
	ocamlc -c Jeu.mli
	ocamlc -c -pp camlp4o Jeu.ml
	
	ocamlc -c mainLettres.ml
	
	ocamlc -o ramideslettres MultiEnsemble.cmo Dictionnaire.cmo Lettres.cmo LoadSaveRami.cmo Jeu.cmo mainLettres.cmo
	
	

clean:
	rm -rf *.cmi *.cmo *~
