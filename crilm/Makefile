SRILM=/s/src/srilm/current
SRILM_INC=-I$(SRILM)/include
SRILM_LIB=-L$(SRILM)/lib/macosx -loolm -lmisc -ldstruct -ltcl -lm
SRILM_CCLIB=-cclib -L$(SRILM)/lib/macosx -cclib '-loolm -lmisc -ldstruct' -cclib -lm

MOD=lmclient
CLASS=lmclass
BASECLASS_INC=-I ..
STUBS=$(MOD)_stubs
BYTES=$(MOD).cma ../baseclient.cmo lmclass.cmo calm.ml 

DEBUG=-DDEBUG

all: byte bin

generate.o: generate.cc
	g++ $(DEBUG) $(SRILM_INC) -o $@ -c $<

stubs: $(STUBS).o
	
$(STUBS).o: $(STUBS).c generate.h
	g++ $(DEBUG) $(SRILM_INC) -I`ocamlc -where` -o $@ -c $<

$(CLASS).cmo: $(CLASS).ml $(MOD).cmo
	ocamlfind ocamlc -thread -package ethread -c $(BASECLASS_INC) $^ -o $@

$(CLASS).cmx: $(CLASS).ml $(MOD).cmx
	ocamlfind ocamlopt -c $(BASECLASS_INC) $^ -o $@
	
dll$(MOD).so: $(STUBS).o generate.o
	ocamlmklib -lstdc++ $(SRILM_LIB) $^ -o $(MOD)

$(MOD).cmo: $(MOD).ml $(STUBS).o
	ocamlc -c $< -o $@

$(MOD).cmx: $(MOD).ml $(STUBS).o
	ocamlmklib -lstdc++ $(SRILM_LIB) $^ -o $(MOD)
	
$(MOD).cma: $(MOD).cmo
	ocamlmklib -o $(MOD) $(MOD).cmo

$(MOD).cmxa: $(MOD).cmx	
	ocamlmklib -o $(MOD) $(MOD).cmx
	
	
byte: dll$(MOD).so $(BYTES)
	ocamlfind ocamlc $(BASECLASS_INC) $(BYTES) -o calm

bin: $(MOD).cmxa generate.o lmclass.cmx calm.ml
	ocamlfind ocamlopt -I . -cclib -lstdc++ $(SRILM_CCLIB) $^ -o calmbin

do:
	ocamlopt lmclient.cmx $(CC_LIB)

start-servers:
	 cd ~/cells
	 ngram -lm 1/mitr-wb5-3-2004-10-01.lm -order 5 -server-port 10101 &
	 ngram -lm 3/mitr-wb5-3-2004-10-01.lm -order 5 -server-port 10103 &
