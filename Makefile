# BUILD RULES
all: 
	ocamlc -c MultiEnsemble.mli
	ocamlc -c MultiEnsemble.ml
	
	ocamlc -c regle.mli
	
	ocamlc -c Dictionnaire.mli
	ocamlc -c -pp camlp4o Dictionnaire.ml
	
	ocamlc -c -pp camlp4o Lettres.ml
	
	ocamlc -c -pp camlp4o LoadSaveRami.ml
	
	

clean:
	rm -rf *.cmi *.cmo *~
