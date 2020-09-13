NAME := shout

LISP     := sbcl
LISPOPTS := --no-sysinit --no-userinit
LISPEXEC := $(LISP) --script

BUILD := build
BUILDAPP = $(BUILD)/buildapp

QLDIR   := $(BUILD)/quicklisp
QLURL   := http://beta.quicklisp.org/quicklisp.lisp
QLFILE  := $(QLDIR)/quicklisp.lisp
QLSETUP := $(QLDIR)/setup.lisp

LISP_QL := $(LISP) $(LISPOPTS) --load $(QLSETUP)

PREFIX = /usr/local
INSTALL_DIR = $(DESTDIR)$(PREFIX)/bin

default: all

$(QLDIR)/setup.lisp:
	@echo "Install Quicklisp"
	mkdir -p $(QLDIR)
	curl -o $(QLFILE) $(QLURL)
ifdef PROXY
	$(LISP) $(LISPOPTS) --load $(QLFILE) \
	  --eval '(quicklisp-quickstart:install :path "$(QLDIR)" :proxy "$(PROXY)")' \
	  --quit
else
	$(LISP) $(LISPOPTS) --load $(QLFILE) \
	  --eval '(quicklisp-quickstart:install :path "$(QLDIR)")' \
	  --quit
endif
	rm $(QLFILE)

quicklisp: $(QLDIR)/setup.lisp ;

$(BUILD)/.reqs:
	mkdir -p $(BUILD)
	@echo "Downloading requirements"
	$(LISP_QL) --eval '(ql:quickload :hunchentoot)'  \
	           --eval '(ql:quickload :drakma)'       \
	           --eval '(ql:quickload :cl-json)'      \
	           --eval '(ql:quickload :daemon)'       \
	           --eval '(ql:quickload :prove)'        \
	           --eval '(load "$(NAME).asd")' --quit
	touch $@

libs: $(BUILD)/.reqs ;

all: shout
shout: quicklisp libs
	$(LISPEXEC) compile.lisp

test: quicklisp libs
	$(LISPEXEC) test.lisp
coverage: quicklisp libs
	$(LISPEXEC) cover.lisp

docker:
	docker build \
	  --build-arg BUILD_DATE="$(shell date -u --iso-8601)" \
	  --build-arg VCS_REF="$(shell git rev-parse --short HEAD)" \
	  -t huntprod/shout .

clean:
	rm -rf $(BUILD)

.PHONY: all test clean
