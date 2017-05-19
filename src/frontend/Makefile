TARGETS ?= scanner.cmo parser.cmo

OCAMLC = ocamlc
OCAMLC_FLAGS = -g
LEX = ocamllex
YACC = ocamlyacc

GEN :=
PHONY :=

PHONY += default
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

PHONY += clean
clean:
	rm -rf *.cmx *.cmi *.cmo *.o $(TARGETS) $(GEN)

.PHONY: $(PHONY)
