include ../platform.mk # SRILM_MACHINE
SRILM_INC=-I$(SRILM)/include
SRILM_LIBDIR=$(SRILM)/lib/$(SRILM_MACHINE)
SRILM_LIB=-L$(SRILM_LIBDIR) -loolm -lmisc -ldstruct -lm #-ltcl
SRILM_CCLIB=-cclib -L$(SRILM_LIBDIR) -cclib '-loolm -lmisc -ldstruct' -cclib -lm

MOD=lmclient
CLASS=lmclass
BASECLASS_INC=-I ..
STUBS=$(MOD)_stubs
BYTES=$(MOD).cma ../baseclient.cmo lmclass.cmo calm.ml 

#DEBUG=-DDEBUG

all: byte bin

generate.o: generate.cc
	g++ $(DEBUG) $(SRILM_INC) -fPIC -o $@ -c $<

stubs: $(STUBS).o
	
$(STUBS).o: $(STUBS).c generate.h
	g++ $(DEBUG) $(SRILM_INC) -I`ocamlc -where` -fPIC -o $@ -c $<

$(CLASS).cmo: $(CLASS).ml $(MOD).cmo
	ocamlfind ocamlc -thread -c $(BASECLASS_INC) $^ -o $@

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

clean:
	rm -f *.o *.cmo *.cmi *.cmx *.a *.so

