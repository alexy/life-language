PROJECT  :=celltimes
LINK_PKG :=pgocaml
COMP_PKG :=pgocaml,pgocaml.statements
CMO      :=.cmo
ML       :=.ml

LMCLIENTA = -I crilm crilm/lmclient.cma
LMCLIENTX = -I crilm crilm/lmclient.cmxa
# compile SRILM with Makefile.machine.macosx, commenting out
# TCL_{INCLUDE,LIBRARY} and adding a line:
# NO_TCL=yes
SRILM=/s/src/srilm/current 
SRILM_CCLIB=-cclib -L$(SRILM)/lib/macosx -cclib '-loolm -lmisc -ldstruct' -cclib -lm

# comment this out to compile without pgocaml available:
USE_POSTGRES=-DUSE_POSTGRES

all: ${PROJECT}

cellspans celltimes: %: %.cmo 
	ocamlfind ocamlc -package $(LINK_PKG) -linkpkg -o $@ $< 

%.cmo: %.ml 
	ocamlfind ocamlc -package $(COMP_PKG) -syntax camlp4o -o $@ -c $<

samplebin.cmx: samplebin.ml
	ocamlfind ocamlopt -package $(COMP_PKG) -pp camlp4o  -o $@ -c $<
	
%.cmx: %.ml
	ocamlfind ocamlopt -package $(COMP_PKG) -syntax camlp4o -o $@ -c $<
	
genlm: genlm.ml
	ocamlfind ocamlc -package unix,str -linkpkg -o $@ $<
	
evalm: evalm.ml
	ocamlfind ocamlc -package unix,str -linkpkg -o $@ $<

sample: evalm.ml sample.ml
	 ocamlfind ocamlc -package unix,pcre,pgocaml -linkpkg percells.cmo $(LMCLIENTA) -o $@ $^
	
samplebin: evalm.cmx samplebin.cmx
	 ocamlfind ocamlopt -package str,unix,pcre -linkpkg -cclib -lstdc++ $(SRILM_CCLIB) $(LMCLIENTX) -o $@  $^

servctl: evalm.ml servctl.ml
	 ocamlfind ocamlc -g -package str,unix,pcre -linkpkg -o $@ $(LMCLIENTA) $^
	
clean:
	rm -f $(PROJECT) $(PROJECT).cmo
	
.SUFFIXES: .cmo .cmi .ml

test-sample:
	./servctl --from=2004-10-01 --ppp=2004-10-01.ppp --base=10000 start
	./sample  --from=2004-10-01 --ppp=2004-10-01.ppp --matrix=percells.bin --take=3 --clients
	./servctl --from=2004-10-01 --ppp=2004-10-01.ppp stop
