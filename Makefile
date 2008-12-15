PROJECT  :=celltimes
LINK_PKG :=pgocaml
PGOC_PKG :=-package pgocaml,pgocaml.statements  -syntax camlp4o
CMO      :=.cmo
ML       :=.ml

DEBUG=-g
LMCLASS_A  = crilm/lmclass.cmo
LMCLIENT_A = -I crilm crilm/lmclient.cma 
LMCLIENT_X = -I crilm crilm/lmclient.cmxa crilm/lmclass.cmx crilm/generate.o
# compile SRILM with Makefile.machine.macosx, commenting out
# TCL_{INCLUDE,LIBRARY} and adding a line:
# NO_TCL=yes
SRILM=/s/src/srilm/current
SRILM_CCLIB=-cclib -L$(SRILM)/lib/macosx -cclib '-loolm -lmisc -ldstruct' -cclib -lm crilm/generate.o

EVALMO=evalm.ml process.cmo baseclient.cmo clclass.cmo syclass.cmo
PARALLEL=parallel
SAMPLO=$(PARALLEL).cmo dataframe.cmo baseclient.cmo process.cmo clclass.cmo syclass.cmo $(LMCLASS_A) evalm.cmo sample.cmo
SAMPLX=${SAMPLO:.cmo=.cmx}

# comment this out to compile without pgocaml available:
USE_POSTGRES=-DUSE_POSTGRES

all: ${PROJECT}

cellspans celltimes: %: %.cmo 
	ocamlfind ocamlc -package $(LINK_PKG) -linkpkg $< -o $@

percells.cmo:
	ocamlfind ocamlc $(PGOC_PKG) -c $< -o $@
	
dataframe.cmo: %.cmo: %.ml
	ocamlfind ocamlc   -pp "camlp4o Camlp4MacroParser.cmo -DONT_USE_POSTGRES" -c $< -o $@

dataframe.cmx: %.cmx: %.ml
	ocamlfind ocamlopt -pp "camlp4o Camlp4MacroParser.cmo -DONT_USE_POSTGRES" -c $< -o $@
 

%.cmo: %.ml 
	ocamlfind ocamlc   -c $< -o $@
	
%.cmx: %.ml
	ocamlfind ocamlopt -c $< -o $@
	
genlm: genlm.ml
	ocamlfind ocamlc -package unix,str -linkpkg $< -o $@ 

baseclient.cmo: %.cmo: %.ml
	ocamlfind ocamlc -thread -package ethread -c $< -o $@
	
#clclass.cmo syclass.cmo crilm/lmclass.cmo: baseclient.cmo
	
evalm.cmo: $(EVALMO) $(LMCLASS_A) 
	ocamlfind ocamlc -thread -package unix,str,ethread -linkpkg -c $(EVALMO) $(LMCLIENT_A) -o $@ 

evalm.cmx: evalm.ml process.cmx clclass.cmo syclass.cmo
	ocamlfind ocamlopt -package unix,str -linkpkg $(LMCLIENT_X) -c $< -o $@ 

sample.cmo: sample.ml $(LMCLASS_A)
	ocamlfind ocamlc $(DEBUG) -package pcre -o $@ -c $<

sample: $(SAMPLO)
	 ocamlfind ocamlc $(DEBUG) -thread -package unix,str,pcre,ethread -linkpkg $^ $(LMCLIENT_A) -cclib -lstdc++ $(SRILM_CCLIB) -o $@ 
	
samplebin: 
	 ocamlfind ocamlopt -thread -package str,unix,pcre,ethread -linkpkg $(LMCLIENT_X) -cclib -lstdc++ $(SRILM_CCLIB) -o $@ $^

servctl: baseclient.cmo process.cmo clclass.cmo syclass.cmo $(LMCLASS_A) evalm.ml servctl.ml
	 ocamlfind ocamlc -g -thread -package str,unix,pcre,ethread -linkpkg $(LMCLIENT_A) -cclib -lstdc++ $(SRILM_CCLIB) -o $@ $^
	
parmap.cmo parallel.cmo: %.cmo: %.ml
	 ocamlfind ocamlc -package unix,pcre -o $@ -c $^

parmap.cmx parallel.cmx: %.cmx: %.ml
	 ocamlfind ocamlopt -package unix,pcre -o $@ -c $^
	
clean:
	rm -f $(PROJECT) $(PROJECT).cmo
	
.SUFFIXES: .cmo .cmi .ml

test-sample:
	./servctl --from=2004-10-01 --ppp=2004-10-01.ppp --base=10000 start
	./sample  --from=2004-10-01 --ppp=2004-10-01.ppp --matrix=percells.bin --take=3 --clients
	./servctl --from=2004-10-01 --ppp=2004-10-01.ppp stop
