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
CC_LIBS = -cclib -lstdc++ $(SRILM_CCLIB)
PARALLEL=parallel

EVALMO=baseclient.cmo process.cmo clclass.cmo syclass.cmo evalm.cmo
CORE_CMOS=$(EVALMO) argv.cmo common.cmo
SAMPLO=$(PARALLEL).cmo dataframe.cmo $(CORE_CMOS) seq.cmo utils.cmo sample.cmo
SERVO=$(CORE_CMOS) servctl.cmo

SAMPLX=${SAMPLO:.cmo=.cmx}

# comment this out to compile without pgocaml available:
USE_POSTGRES=-DUSE_POSTGRES


all: sent

argv.cmo: argv.ml
	ocamlfind ocamlc $(DEBUG) -package pcre -c $^ -o $@
	
sent: argv.cmo seq.cmo utils.cmo common.cmo baseclient.cmo $(LMCLASS_A) generate.ml
	ocamlfind ocamlc $(DEBUG) -package pcre -linkpkg $(LMCLIENT_A) $(CC_LIBS) $^ -o $@
	

cellspans celltimes: %: %.cmo 
	ocamlfind ocamlc -package $(LINK_PKG) -linkpkg $< -o $@

percells.cmo:
	ocamlfind ocamlc $(PGOC_PKG) -c $< -o $@
	
dataframe.cmo: %.cmo: %.ml
	ocamlfind ocamlc   -pp "camlp4o Camlp4MacroParser.cmo -DONT_USE_POSTGRES" -c $< -o $@

dataframe.cmx: %.cmx: %.ml
	ocamlfind ocamlopt -pp "camlp4o Camlp4MacroParser.cmo -DONT_USE_POSTGRES" -c $< -o $@
 

$(LMCLASS_A): $(LMCLASS_A:.cmo=.ml)
	(cd crilm; make lmclass.cmo)
	
%.cmo: %.ml 
	ocamlfind ocamlc $(DEBUG) -c $< -o $@
	
%.cmx: %.ml
	ocamlfind ocamlopt -c $< -o $@
	
genlm: genlm.ml
	ocamlfind ocamlc -package unix,str -linkpkg $< -o $@ 

baseclient.cmo: %.cmo: %.ml
	ocamlfind ocamlc $(DEBUG) -thread -package ethread -c $< -o $@
	
#clclass.cmo syclass.cmo crilm/lmclass.cmo: baseclient.cmo
	
evalm.cmo: $(EVALMO) $(LMCLASS_A) 
	ocamlfind ocamlc $(DEBUG) -thread -package unix,str,ethread -linkpkg -c $(EVALMO) $(LMCLIENT_A) -o $@ 

evalm.cmx: evalm.ml process.cmx clclass.cmo syclass.cmo
	ocamlfind ocamlopt -package unix,str -linkpkg $(LMCLIENT_X) -c $< -o $@ 

sample.cmo: sample.ml $(LMCLASS_A)
	ocamlfind ocamlc $(DEBUG) -package pcre -o $@ -c $<

sample: $(SAMPLO) $(LMCLASS_A)
	 ocamlfind ocamlc $(DEBUG) -thread -package unix,str,pcre,ethread -linkpkg $^ $(LMCLIENT_A) $(CC_LIBS) -o $@ 
	
samplebin: 
	 ocamlfind ocamlopt -thread -package str,unix,pcre,ethread -linkpkg $(LMCLIENT_X) $(CC_LIBS) -o $@ $^

servctl: $(SERVO) $(LMCLASS_A)
	 ocamlfind ocamlc -g -thread -package str,unix,pcre,ethread -linkpkg $(LMCLIENT_A) $(CC_LIBS) -o $@ $^
	
parmap.cmo parallel.cmo: %.cmo: %.ml
	 ocamlfind ocamlc -package unix,pcre -o $@ -c $^

parmap.cmx parallel.cmx: %.cmx: %.ml
	 ocamlfind ocamlopt -package unix,pcre -o $@ -c $^
	
clean:
	rm -f *.cm{o,i,a,xa} sample servctl generate

			
.SUFFIXES: .cmo .cmi .ml

test-sample:
	./servctl --from=2004-10-01 --ppp=2004-10-01.ppp --base=10000 start
	./sample  --from=2004-10-01 --ppp=2004-10-01.ppp --matrix=percells.bin --take=3 --clients
	./servctl --from=2004-10-01 --ppp=2004-10-01.ppp stop
