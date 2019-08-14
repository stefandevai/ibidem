ifeq ($(PREFIX),)
  PREFIX := /usr/local
endif

BIN_PATH = ./bin
EXECUTABLE_NAME = markdown-latex

all: build

.PHONY: build
build:
	sbcl --load markdown-latex.asd \
	     --eval '(ql:quickload :markdown-latex)' \
	     --eval "(sb-ext:save-lisp-and-die \"$(BIN_PATH)/$(EXECUTABLE_NAME)\" :executable t :toplevel 'ml:main :purify t :compression t)" \
	     --end-toplevel-options "$@"

.PHONY: install
install: 
	install -d $(DESTDIR)$(PREFIX)/bin/
	install -m 755 $(BIN_PATH)/$(EXECUTABLE_NAME) $(DESTDIR)$(PREFIX)/bin/

.PHONY: uninstall
uninstall: 
	rm -f $(DESTDIR)$(PREFIX)/bin/$(EXECUTABLE_NAME)

