SRILM=/s/src/srilm/current
SRILM_INC=-I$(SRILM)/include
SRILM_LIB=-L$(SRILM)/lib/macosx -loolm -lmisc -ldstruct -ltcl -lm

all:
	g++ -o lmclient.o $(SRILM_INC) -I`ocamlc -where` -c lmclient.c
	ocamlc -c lmclient.ml
	ocamlmklib -o lmclient -lstdc++ $(SRILM_LIB) lmclient.o
	ocamlmklib -o lmclient lmclient.cmo
	ocamlc -I .   lmclient.cma  calm.ml -o calm
