TARGETS=build_all

FE_DIR = frontend
BE_DIR = backend
OCAMLC = ocamlc
OCAMLC_FLAGS = -g -I $(FE_DIR) $(BE_DIR)
MAKE = make
CLEAN = clean

PHONY :=

PHONY += default
default: $(TARGETS)

PHONY += build_all clean_all
build_all: frontend.build # backend.build
clean_all: frontend.clean # backend.clean

%.build:
	$(MAKE) -C $(@:%.build=%)

%.clean:
	$(MAKE) -C $(@:%.clean=%) $(CLEAN)

.PHONY: $(PHONY)
