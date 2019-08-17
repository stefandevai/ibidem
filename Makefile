ifeq ($(PREFIX),)
  PREFIX := /usr/local
endif

BIN_PATH = ./bin
EXECUTABLE_NAME = latex-builder

MD_FILE_PATH = $(BIN_PATH)/article.md
LATEX_FILE_PATH = $(BIN_PATH)/article.tex
LATEX_OUTPUT_DIR = $(BIN_PATH)/output

all: build

.PHONY: build
build:
	sbcl --load latex-builder.asd \
	     --eval "(ql:quickload :latex-builder)" \
	     --eval "(sb-ext:save-lisp-and-die \"$(BIN_PATH)/$(EXECUTABLE_NAME)\" :executable t :toplevel 'latex-builder:main :purify t :compression t)" \
	     --end-toplevel-options "$@"

.PHONY: install
install:
	install -d $(DESTDIR)$(PREFIX)/bin/
	install -m 755 $(BIN_PATH)/$(EXECUTABLE_NAME) $(DESTDIR)$(PREFIX)/bin/

.PHONY: uninstall
uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/$(EXECUTABLE_NAME)

.PHONY: test
test:
	sbcl --noinform \
		   --disable-debugger \
	     --load latex-builder-test.asd \
		   --eval "(ql:quickload :latex-builder-test)" \
			 --eval "(progn (rove:run :latex-builder-test) (sb-ext:quit))" \

.PHONY: run
run:
	$(BIN_PATH)/$(EXECUTABLE_NAME) $(MD_FILE_PATH) -o $(LATEX_FILE_PATH)
	pdflatex -output-directory $(LATEX_OUTPUT_DIR) $(LATEX_FILE_PATH)
	cp $(LATEX_OUTPUT_DIR)/article.pdf $(BIN_PATH)
