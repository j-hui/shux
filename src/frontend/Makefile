TARGETS=scanner.cmo

OCAMLC=ocamlc
OCAMLC_FLAGS=-I ../src/include -g
LEX=ocamllex
YACC=ocamlyacc
DEP_SRC=exceptions.mli
DEP_OBJ=$(DEP_SRC:%.mli=%.cmi)
AST_SRC=ast.mli
AST_OBJ=$(AST_SRC:%.mli=%.cmi)

GEN= # to be cleaned

.PHONY: default clean
default: $(TARGETS)


GEN += scanner.ml
scanner.ml: scanner.mll
	$(LEX) $<
scanner.cmo: parser.cmi fe_exceptions.cmi


GEN += parser.mli parser.ml
parser.mli parser.ml: parser.mly
	$(YACC) $<
parser.cmi: ast.cmi fe_exceptions.cmi


# general-purpose rules
%.cmo %.cmx: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<
%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

# front-end specific rules
%.output: %.mly
	$(YACC) -v $<

clean:
	rm -rf *.cmx *.cmi *.cmo *.o $(TARGETS) $(GEN)
