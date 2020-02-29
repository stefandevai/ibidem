ifeq ($(PREFIX),)
  PREFIX := /usr/local
endif

BUILD_PATH = ./build
EXECUTABLE_NAME = latex-builder
CURRENT_DIR_STRING = $(CURDIR)/

all: build

.PHONY: build
build:
	mkdir -p $(BUILD_PATH)
	sbcl --eval "(require \"asdf\")" \
		   --eval "(push #p\"$(CURRENT_DIR_STRING)\" asdf:*central-registry*)" \
	     --eval "(asdf:make :latex-builder)" \
			 --eval "(quit)"

.PHONY: release
release:
	mkdir -p $(BUILD_PATH)
	sbcl --eval "(require \"asdf\")" \
		   --eval "(push #p\"$(CURRENT_DIR_STRING)\" asdf:*central-registry*)" \
			 --eval "(asdf:load-system :latex-builder)" \
			 --eval "(sb-ext:save-lisp-and-die \"$(BUILD_PATH)/$(EXECUTABLE_NAME)\" :executable t :toplevel 'latex-builder:main :purify t :compression t)" \
			 --end-toplevel-options "$@"

.PHONY: clean
clean:
	rm -rf $(BUILD_PATH)
	sbcl --eval "(asdf:clear-system :latex-builder)" \
			 --eval "(quit)"

.PHONY: install
install:
	install -d $(DESTDIR)$(PREFIX)/bin/
	install -m 755 $(BUILD_PATH)/$(EXECUTABLE_NAME) $(DESTDIR)$(PREFIX)/bin/

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
