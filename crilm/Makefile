SRILM=/s/src/srilm/current
SRILM_INC=-I$(SRILM)/include
SRILM_LIB=-L$(SRILM)/lib/macosx -loolm -lmisc -ldstruct -ltcl -lm
SRILM_CCLIB=-cclib -L$(SRILM)/lib/macosx -cclib '-loolm -lmisc -ldstruct' -cclib -lm

MOD=lmclient
STUBS=$(MOD)_stubs

all: byte bin

$(STUBS).o: $(STUBS).c
	g++ $(SRILM_INC) -I`ocamlc -where` -o $@ -c $<

byte: $(STUBS).o
	ocamlc     -c $(MOD).ml
	ocamlmklib -o $(MOD) -lstdc++ $(SRILM_LIB) $(STUBS).o
	ocamlmklib -o $(MOD) $(MOD).cmo
	ocamlc -I .   $(MOD).cma calm.ml -o calm

bin: $(STUBS).o
	ocamlopt   -c $(MOD).ml
	ocamlmklib -o $(MOD) -lstdc++ $(SRILM_LIB) $(STUBS).o
	ocamlmklib -o $(MOD) $(MOD).cmx
	ocamlfind ocamlopt -I . -cclib -lstdc++ $(SRILM_CCLIB) $(MOD).cmxa calm.ml -o calmbin

do:
	ocamlopt lmclient.cmx $(CC_LIB)

start-servers:
	 cd ~/cells
	 ngram -lm 1/mitr-wb5-3-2004-10-01.lm -order 5 -server-port 10101 &
	 ngram -lm 3/mitr-wb5-3-2004-10-01.lm -order 5 -server-port 10103 &
	
