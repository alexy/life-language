SRILM=/s/src/srilm/current
SRILM_INC=-I$(SRILM)/include
SRILM_LIB=-L$(SRILM)/lib/macosx -loolm -lmisc -ldstruct -ltcl -lm
SRILM_CCLIB=-cclib -L$(SRILM)/lib/macosx -cclib '-loolm -lmisc -ldstruct' -cclib -lm

MOD=lmclient
CLASS=lmclass
STUBS=$(MOD)_stubs

all: byte bin

generate.o: generate.cc
	g++ $(SRILM_INC) -o $@ -c $<

stubs: $(STUBS).o
	
$(STUBS).o: $(STUBS).c generate.h
	g++ $(SRILM_INC) -I`ocamlc -where` -o $@ -c $<

$(CLASS).cmo: $(CLASS).ml $(MOD).cmo
	ocamlc -c $^ -o $@
	
dll$(MOD).so: $(STUBS).o generate.o
	ocamlmklib -lstdc++ $(SRILM_LIB) generate.o -o $(MOD) $<

$(MOD).cmo: $(MOD).ml $(STUBS).o
	ocamlc -c $< -o $@
	
$(MOD).cma: $(MOD).cmo
	ocamlmklib -o $(MOD) $(MOD).cmo
	
	
byte: dll$(MOD).so $(MOD).cma lmclass.cmo calm.ml
	ocamlc -I . $(MOD).cma lmclass.cmo calm.ml -o calm

bin: $(STUBS).o
	ocamlmklib -o $(MOD) -lstdc++ $(SRILM_LIB) $(STUBS).o
	ocamlmklib -o $(MOD) $(MOD).cmx
	ocamlfind ocamlopt -I . -cclib -lstdc++ $(SRILM_CCLIB) $(MOD).cmxa calm.ml -o calmbin

do:
	ocamlopt lmclient.cmx $(CC_LIB)

start-servers:
	 cd ~/cells
	 ngram -lm 1/mitr-wb5-3-2004-10-01.lm -order 5 -server-port 10101 &
	 ngram -lm 3/mitr-wb5-3-2004-10-01.lm -order 5 -server-port 10103 &
	
