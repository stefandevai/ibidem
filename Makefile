ifeq ($(PREFIX),)
  PREFIX := /usr/local
endif

BUILD_PATH = ./build
EXECUTABLE_NAME = ibidem
CURRENT_DIR_STRING = $(CURDIR)/

all: build

.PHONY: build
build:
	mkdir -p $(BUILD_PATH)
	sbcl --eval "(require \"asdf\")" \
		   --eval "(push #p\"$(CURRENT_DIR_STRING)\" asdf:*central-registry*)" \
	     --eval "(asdf:make :ibidem)" \
			 --eval "(quit)"

.PHONY: release
release:
	mkdir -p $(BUILD_PATH)
	sbcl --eval "(require \"asdf\")" \
		   --eval "(push #p\"$(CURRENT_DIR_STRING)\" asdf:*central-registry*)" \
			 --eval "(asdf:load-system :ibidem)" \
			 --eval "(sb-ext:save-lisp-and-die \"$(BUILD_PATH)/$(EXECUTABLE_NAME)\" :executable t :toplevel 'ibidem:main :purify t :compression t)" \
			 --end-toplevel-options "$@"

.PHONY: clean
clean:
	rm -rf $(BUILD_PATH)
	sbcl --eval "(asdf:clear-system :ibidem)" \
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
	     --load ibidem-test.asd \
		   --eval "(ql:quickload :ibidem-test)" \
			 --eval "(progn (rove:run :ibidem-test) (sb-ext:quit))" \
