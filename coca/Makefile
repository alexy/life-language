all: caller.opt

caller.opt:
	g++ -o mymod_stubs.o -I`ocamlc -where` -c mymod_stubs.cc
	ocamlopt -c mymod.ml
	ocamlmklib -o mymod mymod_stubs.o
	ocamlmklib -o mymod mymod.cmx
	ocamlopt -I . -cclib -lstdc++ mymod.cmxa caller.ml -o caller.opt


caller:
	g++ -o mymod_stubs.o -I`ocamlc -where` -c mymod_stubs.cc
	ocamlc -c mymod.ml
	ocamlmklib -o mymod -lstdc++ mymod_stubs.o
	ocamlmklib -o mymod  mymod.cmo
	ocamlc -I . mymod.cma  caller.ml  -o caller.byte

run1:
	ocaml mymod.cma caller.ml

run2:
	ocamlrun -I . caller.byte

run.opt:
	./caller.opt