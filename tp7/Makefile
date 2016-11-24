CMO=turtle.cmo lexer.cmo parser.cmo interp.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli
BIN=mini-turtle
FLAGS=

all: $(BIN)
	./$(BIN) test.logo

.PHONY: tests

tests: $(BIN)
	for f in tests/*.logo; do ./$(BIN) $$f; done

$(BIN): $(CMO)
	ocamlc $(FLAGS) -o $(BIN) graphics.cma $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir --infer -v $<

clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) parser.automaton

parser.ml: ast.cmi

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend



