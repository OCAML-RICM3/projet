# PROJECT
DEMOS=

PLIBS=aff geo
PCMAS=$(foreach lib, $(PLIBS), $(addprefix $(lib),.cma))

# ENVIRONMENT
OCAMLC=ocamlc
INCLUDES=-I +lablGL
OCAMLFLAGS=$(INCLUDES)
LABLGLCMAS=lablgl.cma lablglut.cma unix.cma

# BUILD RULES
all: $(DEMOS)

%: $(PCMAS) %.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LABLGLCMAS) $+

geo.cma: geo.ml geo.cmi
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -a $<
geo.cmi: geo.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<
geo.cmo: geo.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

aff.cma: aff.ml aff.cmi geo.cma
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -a $(LABLGLCMAS) geo.cma $<
aff.cmi: aff.mli geo.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $<
aff.cmo: aff.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $+

clean:
	rm -vf $(foreach demo, $(DEMOS), $(addprefix $(demo),.cmi .cmo))
	rm -vf $(foreach lib, $(PLIBS), $(addprefix $(lib),.cma .cmi .cmo))
mrproper: clean
	rm -vf $(DEMOS)

.PHONY: all clean mrproper
