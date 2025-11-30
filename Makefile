-include .config.mk

PKG = llama

ELS   = $(PKG).el
ELS  += $(PKG)-tests.el
ELCS  = $(ELS:.el=.elc)

$(PKG).elc:
$(PKG)-tests.elc: $(PKG).elc

DEPS  = compat


LOAD_PATH ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH += -L .

EMACS       ?= emacs
EMACS_ARGS ?= --eval "(progn \
  (put 'if-let 'byte-obsolete-info nil) \
  (put 'when-let 'byte-obsolete-info nil))"
EMACS_Q_ARG ?= -Q
EMACS_BATCH ?= $(EMACS) $(EMACS_Q_ARG) --batch $(EMACS_ARGS) $(LOAD_PATH)

all: lisp

help:
	$(info make all      -- Build lisp)
	$(info make lisp     -- Build lisp)
	$(info make redo     -- Build lisp from scratch)
	$(info make test     -- Run tests)
	$(info make clean    -- Remove built files)
	@printf "\n"

redo: clean lisp

lisp: $(ELCS) autoloads check-declare

autoloads: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS_BATCH) --eval "(check-declare-directory default-directory)"

test: lisp
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) \
	-l ert -l $(PKG)-tests.el -f ert-run-tests-batch-and-exit

CLEAN = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning...\n"
	@rm -rf $(CLEAN)

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS_BATCH) --load autoload --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"
