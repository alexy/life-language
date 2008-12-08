PROJECT  :=celltimes
LINK_PKG :=pgocaml
PGOC_PKG :=-package pgocaml,pgocaml.statements  -syntax camlp4o
CMO      :=.cmo
ML       :=.ml

DEBUG=-g
LMCLIENT_A = -I crilm crilm/lmclient.cma
LMCLIENT_X = -I crilm crilm/lmclient.cmxa
# compile SRILM with Makefile.machine.macosx, commenting out
# TCL_{INCLUDE,LIBRARY} and adding a line:
# NO_TCL=yes
SRILM=/s/src/srilm/current
SRILM_CCLIB=-cclib -L$(SRILM)/lib/macosx -cclib '-loolm -lmisc -ldstruct' -cclib -lm

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
	ocamlfind ocamlc -c $< -o $@
	
%.cmx: %.ml
	ocamlfind ocamlopt -package $(COMP_PKG) -syntax camlp4o -c $< -o $@
	
genlm: genlm.ml
	ocamlfind ocamlc -package unix,str -linkpkg $< -o $@ 
	
evalm.cmo: evalm.ml
	ocamlfind ocamlc   -package unix,str -linkpkg $(LMCLIENT_A) -c $< -o $@ 

evalm.cmx: evalm.ml
	ocamlfind ocamlopt -package unix,str -linkpkg $(LMCLIENT_X) -c $< -o $@ 


sample:    evalm.cmo dataframe.cmo sample.ml
	 ocamlfind ocamlc $(DEBUG) -package unix,str,pcre -linkpkg $(LMCLIENT_A) -cclib -lstdc++ $(SRILM_CCLIB) -o $@ $^ 
	
samplebin: evalm.cmx dataframe.cmx sample.ml
	 ocamlfind ocamlopt        -package str,unix,pcre -linkpkg $(LMCLIENT_X) -cclib -lstdc++ $(SRILM_CCLIB) -o $@ $^

servctl: evalm.ml servctl.ml
	 ocamlfind ocamlc -g -package str,unix,pcre -linkpkg -o $@ $(LMCLIENTA) $^
	
clean:
	rm -f $(PROJECT) $(PROJECT).cmo
	
.SUFFIXES: .cmo .cmi .ml

test-sample:
	./servctl --from=2004-10-01 --ppp=2004-10-01.ppp --base=10000 start
	./sample  --from=2004-10-01 --ppp=2004-10-01.ppp --matrix=percells.bin --take=3 --clients
	./servctl --from=2004-10-01 --ppp=2004-10-01.ppp stop
