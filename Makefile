-include .config.mk

PKG = llama

ELS   = $(PKG).el
ELS  += $(PKG)-test.el
ELCS  = $(ELS:.el=.elc)

$(PKG).elc:
$(PKG)-test.elc: $(PKG).elc

DEPS  = compat

EMACS      ?= emacs
EMACS_ARGS ?= --eval "(progn \
  (put 'if-let 'byte-obsolete-info nil) \
  (put 'when-let 'byte-obsolete-info nil))"

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

all: lisp

help:
	$(info make all          - generate byte-code and autoloads)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make redo         - re-generate byte-code and autoloads)
	$(info make test         - run tests)
	$(info make clean        - remove generated files)
	@printf "\n"

redo: clean lisp

lisp: $(ELCS) loaddefs check-declare

loaddefs: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) \
	--eval "(check-declare-directory default-directory)"

test: lisp
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) \
	-l ert -l $(PKG)-test.el -f ert-run-tests-batch-and-exit

CLEAN  = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning...\n"
	@rm -rf $(CLEAN)

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS) -Q --batch -l autoload -l cl-lib --eval "\
(let ((file (expand-file-name \"$@\"))\
      (autoload-timestamps nil) \
      (backup-inhibited t)\
      (version-control 'never)\
      (coding-system-for-write 'utf-8-emacs-unix))\
  (write-region (autoload-rubric file \"package\" nil) nil file nil 'silent)\
  (cl-letf (((symbol-function 'progress-reporter-do-update) (lambda (&rest _)))\
            ((symbol-function 'progress-reporter-done) (lambda (_))))\
    (let ((generated-autoload-file file))\
      (update-directory-autoloads default-directory))))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"
