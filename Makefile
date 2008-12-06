PROJECT  :=celltimes
LINK_PKG :=pgocaml
COMP_PKG :=pgocaml,pgocaml.statements
CMO      :=.cmo
ML       :=.ml

all: ${PROJECT}

cellspans celltimes: %: %.cmo 
	ocamlfind ocamlc -package $(LINK_PKG) -linkpkg -o $@ $< 

%.cmo: %.ml 
	ocamlfind ocamlc -package $(COMP_PKG) -syntax camlp4o -o $@ -c $<
	
genlm: genlm.ml
	ocamlfind ocamlc -package unix,str -linkpkg -o $@ $<
	
evalm: evalm.ml
	ocamlfind ocamlc -package unix,str -linkpkg -o $@ $<

sample: evalm.ml sample.ml
	 ocamlfind ocamlc -package unix,pcre,pgocaml -linkpkg -o $@ percells.cmo $^
	
samplebin: evalm.ml samplebin.ml
	 ocamlfind ocamlopt -package str,unix,pcre -linkpkg -o $@ $^

servctl: evalm.ml servctl.ml
	 ocamlfind ocamlc -g -package str,unix,pcre -linkpkg -o $@ $^
	
clean:
	rm -f $(PROJECT) $(PROJECT).cmo
	
.SUFFIXES: .cmo .cmi .ml