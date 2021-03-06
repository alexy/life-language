include platform.mk
PROJECT  :=celltimes
LINK_PKG :=pgocaml
PGOC_PKG :=-package pgocaml,pgocaml.statements  -syntax camlp4o
CMO      :=.cmo
ML       :=.ml

DEBUG=-g

LMCLASS_A  = crilm/lmclass.cmo
LMCLIENT_A = -I crilm crilm/lmclient.cma 
LMCLIENT_X = -I crilm crilm/lmclient.cmxa crilm/generate.o
# compile SRILM with Makefile.machine.macosx, commenting out
# TCL_{INCLUDE,LIBRARY} and adding a line:
# NO_TCL=yes
SRILM_CCLIB=-cclib -L$(SRILM)/lib/$(SRILM_MACHINE) -cclib '-loolm -lmisc -ldstruct' -cclib -lm
CC_LIBS = -cclib -lstdc++ $(SRILM_CCLIB)
PARALLEL=parallel

CLASSO=baseclient.cmo process.cmo clclass.cmo syclass.cmo
EVALMO=$(CLASSO) $(LMCLASS_A)
CORE_CMOS=$(EVALMO) utils.cmo common.cmo evalm.cmo
SAMPLO=$(PARALLEL).cmo dataframe.cmo $(CORE_CMOS) seq.cmo
SERVO=$(CORE_CMOS) servctl.cmo

SAMPLX=${SAMPLO:.cmo=.cmx}

SUFFIX_DIR=/s/src/ocaml/suffix/ferre
SUFFIX_BASE=cis lSet suffix intseq
SUFFIX_CMOS_FULL=${SUFFIX_BASE:%=$(SUFFIX_DIR)/%.cmo}
SUFFIX_CMOS=${SUFFIX_BASE:%=%.cmo}
SUFFIX_CMXS=${SUFFIX_BASE:%=%.cmx}

# comment this out to compile without pgocaml available:
USE_POSTGRES=-DUSE_POSTGRES


all: sample.opt

utils.cmo seq.cmo: %.cmo: %.ml
	ocamlfind ocamlc $(DEBUG) -package pcre -c $^ -o $@

utils.cmx seq.cmx: %.cmx: %.ml
	ocamlfind ocamlopt $(DEBUG) -package pcre -c $^ -o $@
	
sent: seq.cmo utils.cmo common.cmo baseclient.cmo $(LMCLASS_A) generate.ml
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
	
fertilize: seq.cmo utils.cmo fertilize.ml
	ocamlfind ocamlc $(DEBUG) -package str,pcre -linkpkg $^ -o $@
	
fertilize.opt: seq.cmx utils.cmx fertilize.ml
	ocamlfind ocamlopt $(DEBUG) -package str,pcre -linkpkg $^ -o $@
	
treeru: seq.cmo utils.cmo treeru.ml
	echo suffix objects: $(SUFFIX_CMOS)
	ocamlfind ocamlc $(DEBUG) -package str,pcre -linkpkg -I $(SUFFIX_DIR) $(SUFFIX_CMOS) $^ -o $@

treeru.opt: seq.cmx utils.cmx treeru.ml
	echo suffix objects: $(SUFFIX_CMXS)
	ocamlfind ocamlopt -package str,pcre -linkpkg -I $(SUFFIX_DIR) $(SUFFIX_CMXS) $^ -o $@
	
unis: seq.cmo unis.ml 
	ocamlfind ocamlc -package unix,str -linkpkg $^ -o $@ 

genlm: seq.cmo genlm.ml 
	ocamlfind ocamlc -package unix,str,pcre -linkpkg $^ -o $@ 

genlm.opt: seq.cmx genlm.ml 
	ocamlfind ocamlopt -package unix,str,pcre -linkpkg $^ -o $@ 

baseclient.cmo: %.cmo: %.ml
	ocamlfind ocamlc $(DEBUG) -thread -c $< -o $@
	
#clclass.cmo syclass.cmo crilm/lmclass.cmo: baseclient.cmo
	
evalm.cmo: evalm.ml $(EVALMO) $(LMCLASS_A)
	ocamlfind ocamlc $(DEBUG) -package unix,str -linkpkg $(EVALMO) $(LMCLIENT_A) -c $< -o $@ 

evalm.cmx: evalm.ml process.cmx clclass.cmo syclass.cmo
	ocamlfind ocamlopt -package unix,str -linkpkg $(LMCLIENT_X) -c $< -o $@ 

sample.cmo: sample.ml $(LMCLASS_A)
	ocamlfind ocamlc $(DEBUG) -package pcre -o $@ -c $<

sample: $(SAMPLO) $(LMCLASS_A) sample.ml
	 ocamlfind ocamlc $(DEBUG) -package unix,str,pcre -linkpkg $(LMCLIENT_A) $^ $(CC_LIBS) -o $@ 
	
sample.opt: $(SAMPLX) $(LMCLASS_X) sample.ml 
	 ocamlfind ocamlopt -thread -package str,unix,pcre -linkpkg  $^ $(LMCLIENT_X) $(CC_LIBS) -o $@

servctl: $(SERVO) $(LMCLASS_A)
	 ocamlfind ocamlc -g -thread -package str,unix,pcre -linkpkg $(LMCLIENT_A) $(CC_LIBS) -o $@ $^
	
parmap.cmo parallel.cmo: %.cmo: %.ml
	 ocamlfind ocamlc -package unix,pcre -o $@ -c $^

parmap.cmx parallel.cmx: %.cmx: %.ml
	 ocamlfind ocamlopt -package unix,pcre -o $@ -c $^
	
clean:
	rm -f *.cm{o,i,a,xa} sample servctl generate

			
.SUFFIXES: .cmo .cmi .ml


start:
	./servctl --ppp=2004-10-01.ppp --base=10000 --order=5 --maxclients=2 start

run:
	./sample  --ppp=2004-10-01.ppp --batch=10 --take=3 --clients --parallel

stop:
	./servctl --ppp=2004-10-01.ppp stop

kill:
	 ps auxwww | grep ngram | cut -d' ' -f4 | xargs kill
