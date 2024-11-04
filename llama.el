;;; llama.el --- Compact syntax for short lambda  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Jonas Bernoulli

;; Authors: Jonas Bernoulli <emacs.llama@jonas.bernoulli.dev>
;; Homepage: https://github.com/tarsius/llama
;; Keywords: extensions

;; Package-Version: 0.4.0
;; Package-Requires: ((emacs "26.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a macro named `##', which provides a compact way
;; to write short `lambda' expressions.

;; The signature of the macro is (## FN &rest BODY) and it expands to a
;; `lambda' expression, which calls the function FN with the arguments BODY
;; and returns the value of that.  The arguments of the `lambda' expression
;; are derived from symbols found in BODY.

;; Each symbol from `%1' through `%9', which appears in an unquoted part
;; of BODY, specifies a mandatory argument.  Each symbol from `&1' through
;; `&9', which appears in an unquoted part of BODY, specifies an optional
;; argument.  The symbol `&*' specifies extra (`&rest') arguments.

;; The shorter symbol `%' can be used instead of `%1', but using both in
;; the same expression is not allowed.  Likewise `&' can be used instead
;; of `&1'.  These shorthands are not recognized in function position.

;; To support binding forms that use a vector as VARLIST (such as `-let'
;; from the `dash' package), argument symbols are also detected inside of
;; vectors.

;; The space between `##' and FN can be omitted because `##' is read-syntax
;; for the symbol whose name is the empty string.  If you prefer you can
;; place a space there anyway, and if you prefer to not use this somewhat
;; magical symbol at all, you can instead use the alternative name `llama'.

;; Instead of:
;;
;;   (lambda (a &optional _ c &rest d)
;;     (foo a (bar c) d))
;;
;; you can use this macro and write:
;;
;;   (##foo %1 (bar &3) &*)
;;
;; which expands to:
;;
;;   (lambda (%1 &optional _&2 &3 &rest &*)
;;     (foo %1 (bar &3) &*))

;; Unused trailing arguments and mandatory unused arguments at the border
;; between mandatory and optional arguments are also supported:
;;
;;   (##list %1 _%3 &5 _&6)
;;
;; becomes:
;;
;;   (lambda (%1 _%2 _%3 &optional _&4 &5 _&6)
;;     (list %1 &5))
;;
;; Note how `_%3' and `_&6' are removed from the body, because their names
;; begin with an underscore.  Also note that `_&4' is optional, unlike the
;; explicitly specified `_%3'.

;;; Code:

;;;###autoload
(defmacro llama (fn &rest body)
  "Expand to a `lambda' expression that wraps around FN and BODY.

This macro provides a compact way to write short `lambda' expressions.
It expands to a `lambda' expression, which calls the function FN with
arguments BODY and returns its value.  The arguments of the `lambda'
expression are derived from symbols found in BODY.

Each symbol from `%1' through `%9', which appears in an unquoted part
of BODY, specifies a mandatory argument.  Each symbol from `&1' through
`&9', which appears in an unquoted part of BODY, specifies an optional
argument.  The symbol `&*' specifies extra (`&rest') arguments.

The shorter symbol `%' can be used instead of `%1', but using both in
the same expression is not allowed.  Likewise `&' can be used instead
of `&1'.  These shorthands are not recognized in function position.

To support binding forms that use a vector as VARLIST (such as `-let'
from the `dash' package), argument symbols are also detected inside of
vectors.

The space between `##' and FN can be omitted because `##' is read-syntax
for the symbol whose name is the empty string.  If you prefer you can
place a space there anyway, and if you prefer to not use this somewhat
magical symbol at all, you can instead use the alternative name `llama'.

Instead of:

  (lambda (a &optional _ c &rest d)
    (foo a (bar c) d))

you can use this macro and write:

  (##foo %1 (bar &3) &*)

which expands to:

  (lambda (%1 &optional _&2 &3 &rest &*)
    (foo %1 (bar &3) &*))

Unused trailing arguments and mandatory unused arguments at the border
between mandatory and optional arguments are also supported:

  (##list %1 _%3 &5 _&6)

becomes:

  (lambda (%1 _%2 _%3 &optional _&4 &5 _&6)
    (list %1 &5))

Note how `_%3' and `_&6' are removed from the body, because their names
begin with an underscore.  Also note that `_&4' is optional, unlike the
explicitly specified `_%3'."
  (cond ((symbolp fn))
        ((and (eq (car-safe fn) backquote-backquote-symbol)
              (not body))
         (setq body (cdr fn))
         (setq fn backquote-backquote-symbol))
        ((signal 'wrong-type-argument
                 (list 'symbolp backquote-backquote-symbol fn))))
  (let* ((args (make-vector 10 nil))
         (body (cdr (llama--collect (cons fn body) args)))
         (rest (aref args 0))
         (args (nreverse (cdr (append args nil))))
         (args (progn (while (and args (null (car args)))
                        (setq args (cdr args)))
                      args))
         (pos  (length args))
         (opt  nil)
         (args (mapcar
                (lambda (arg)
                  (if arg
                      (setq opt (string-match-p "\\`_?&" (symbol-name arg)))
                    (setq arg (intern (format "_%c%s" (if opt ?& ?%) pos))))
                  (setq pos (1- pos))
                  arg)
                args))
         (opt  nil)
         (args (mapcar
                (lambda (symbol)
                  (cond
                   ((string-match-p "\\`_?%" (symbol-name symbol))
                    (when opt
                      (error "`%s' cannot follow optional arguments" symbol))
                    (list symbol))
                   (opt
                    (list symbol))
                   ((setq opt t)
                    (list '&optional symbol))))
                (nreverse args))))
    `(lambda
       (,@(apply #'nconc args)
        ,@(and rest (list '&rest rest)))
       (,fn ,@body))))

(defalias (intern "") 'llama)
(defalias '\#\# 'llama)

(defconst llama--unused-argument (make-symbol "llama--unused-argument"))

(defun llama--collect (expr args &optional fnpos backquoted unquote)
  (cond
   ((memq (car-safe expr) (list (intern "") 'llama 'quote)) expr)
   ((and backquoted (symbolp expr)) expr)
   ((and backquoted
         (memq (car-safe expr)
               (list backquote-unquote-symbol
                     backquote-splice-symbol)))
    (list (car expr)
          (llama--collect (cadr expr) args nil nil t)))
   ((memq (car-safe expr)
          (list backquote-backquote-symbol
                backquote-splice-symbol))
    (list (car expr)
          (llama--collect (cadr expr) args nil t)))
   ((symbolp expr)
    (let ((name (symbol-name expr)))
      (save-match-data
        (cond
         ((string-match "\\`\\(_\\)?[%&]\\([1-9*]\\)?\\'" name)
          (let* ((pos (match-string 2 name))
                 (pos (cond ((equal pos "*") 0)
                            ((not pos) 1)
                            ((string-to-number pos))))
                 (sym (aref args pos)))
            (unless (and fnpos (not unquote) (memq expr '(% &)))
              (when (and sym (not (equal expr sym)))
                (error "`%s' and `%s' are mutually exclusive" sym expr))
              (aset args pos expr)))
          (if (match-string 1 name)
              llama--unused-argument
            expr))
         (expr)))))
   ((or (listp expr)
        (vectorp expr))
    (let* ((vectorp (vectorp expr))
           (expr (if vectorp (append expr ()) expr))
           (fnpos (and (not vectorp)
                       (not backquoted)
                       (ignore-errors (length expr)))) ;proper-list-p
           (ret ()))
      (catch t
        (while t
          (let ((elt (llama--collect (car expr) args fnpos backquoted)))
            (unless (eq elt llama--unused-argument)
              (push elt ret)))
          (setq fnpos nil)
          (setq expr (cdr expr))
          (unless (and expr
                       (listp expr)
                       (not (eq (car expr) backquote-unquote-symbol)))
            (throw t nil))))
      (setq ret (nreverse ret))
      (when expr
        (setcdr (last ret) (llama--collect expr args nil backquoted)))
      (if vectorp (vconcat ret) ret)))
   (expr)))

;;; Advices

(define-advice elisp--expect-function-p (:around (fn pos) llama)
  "Support function completion directly following `##'."
  (or (and (eq (char-before    pos)    ?#)
           (eq (char-before (- pos 1)) ?#))
      (and (eq (char-before    pos)    ?\s)
           (eq (char-before (- pos 1)) ?#)
           (eq (char-before (- pos 2)) ?#))
      (funcall fn pos)))

(define-advice elisp-mode-syntax-propertize (:override (start end) llama)
  ;; Synced with Emacs up to 6b9510d94f814cacf43793dce76250b5f7e6f64a.
  "Like `elisp-mode-syntax-propertize' but don't change syntax of `##'."
  (goto-char start)
  (let ((case-fold-search nil))
    (funcall
     (syntax-propertize-rules
      ;; Empty symbol.
      ;; {{ Comment out to prevent the `##' from becoming part of
      ;;    the following symbol when there is no space in between.
      ;; ("##" (0 (unless (nth 8 (syntax-ppss))
      ;;            (string-to-syntax "_"))))
      ;; }}
      ;; {{ As for other symbols, use `font-lock-constant-face' in
      ;;    docstrings and comments.
      ("##" (0 (when (nth 8 (syntax-ppss))
                 (string-to-syntax "_"))))
      ;; }}
      ;; Prevent the @ from becoming part of a following symbol.
      ;; {{ Preserve this part, even though it is absent from
      ;;    this function in 29.1; backporting it by association.
      (",@" (0 (unless (nth 8 (syntax-ppss))
                 (string-to-syntax "'"))))
      ;; }}
      ;; Unicode character names.  (The longest name is 88 characters
      ;; long.)
      ("\\?\\\\N{[-A-Za-z0-9 ]\\{,100\\}}"
       (0 (unless (nth 8 (syntax-ppss))
            (string-to-syntax "_"))))
      ((rx "#" (or (seq (group-n 1 "&" (+ digit)) ?\") ; Bool-vector.
                   (seq (group-n 1 "s") "(")           ; Record.
                   (seq (group-n 1 (+ "^")) "[")))     ; Char-table.
       (1 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
            (string-to-syntax "'")))))
     start end)))

(define-advice completing-read (:around (fn &rest args) llama)
  "Unintern the symbol with the empty name during completion.

`##' is the notation for the symbol whose name is the empty string.
  (intern \"\") => ##
  (symbol-name \\='##) => \"\"

The `llama' package uses `##' as the name of a macro, which allows
it to be used akin to syntax, without actually being new syntax.
\(`describe-function' won't let you select `##', but because that is an
alias for `llama', you can access the documentation under that name.)

This advice prevents the empty string from being offered as a completion
candidate when `obarray' (or a completion table that internally uses
that) is used as COLLECTION, by `unintern'ing that symbol temporarily."
  (let ((plist (symbol-plist '##))
        (value nil)
        (bound nil))
    (with-no-warnings
      (when (boundp '##)
        (setq bound t)
        (setq value ##)))
    (unwind-protect
        (progn (unintern "" obarray)
               (apply fn args))
      (defalias (intern "") 'llama)
      (setplist (intern "") plist)
      (when bound
        (set (intern "") value)))))

;;; Fontification

(defgroup llama ()
  "Compact syntax for short lambda."
  :group 'extensions
  :group 'lisp)

(defface llama-\#\#-macro '((t :inherit font-lock-function-call-face))
  "Face used for the name of the `##' macro.")

(defface llama-llama-macro '((t :inherit font-lock-keyword-face))
  "Face used for the name of the `llama' macro.")

(defface llama-mandatory-argument '((t :inherit font-lock-variable-use-face))
  "Face used for mandatory arguments `%1' through `%9' and `%'.")

(defface llama-optional-argument '((t :inherit font-lock-type-face))
  "Face used for optional arguments `&1' through `&9', `&' and `&*'.")

(defface llama-deleted-argument
  `((((supports :box t))
     :box ( :line-width ,(if (>= emacs-major-version 28) (cons -1 -1) -1)
            :color "red"
            :style nil))
    (((supports :underline t))
     :underline "red")
    (t
     :inherit font-lock-warning-face))
  "Face used for deleted arguments `_%1'...`_%9', `_&1'...`_&9' and `_&*'.
This face is used in addition to one of llama's other argument faces.
Unlike implicit unused arguments (which do not appear in the function
body), these arguments are deleted from the function body during macro
expansion, and the looks of this face should hint at that.")

(defconst llama-font-lock-keywords-28
  '(("(\\(##\\)" 1 'llama-\#\#-macro)
    ("(\\(llama\\)\\_>" 1 'llama-llama-macro)
    ("\\_<\\(?:_?%[1-9]?\\)\\_>"
     0 (llama--maybe-face 'llama-mandatory-argument))
    ("\\_<\\(?:_?&[1-9*]?\\)\\_>"
     0 (llama--maybe-face 'llama-optional-argument))
    ("\\_<\\(?:_\\(?:%[1-9]?\\|&[1-9*]?\\)\\)\\_>"
     0 'llama-deleted-argument prepend)))

(defconst llama-font-lock-keywords-29
  `(("\\_<\\(&[1-9*]?\\)\\_>" 1 'default)
    (,(apply-partially #'llama--match-and-fontify "(\\(##\\)")
     1 'llama-\#\#-macro)
    (,(apply-partially #'llama--match-and-fontify "(\\(llama\\_>\\)")
     1 'llama-llama-macro)))

(defvar llama-font-lock-keywords
  (if (fboundp 'read-positioning-symbols)
      llama-font-lock-keywords-29
    llama-font-lock-keywords-28))

(defun llama--maybe-face (face)
  (and (not (and (member (match-string 0) '("%" "&"))
                 (and-let* ((beg (ignore-errors
                                   (scan-lists (match-beginning 0) -1 1))))
                   (string-match-p "\\`\\(##\\|llama\\_>\\)?[\s\t\n\r]*\\'"
                                   (buffer-substring-no-properties
                                    (1+ beg) (match-beginning 0))))))
       face))

(defun llama--match-and-fontify (re end)
  (and (re-search-forward re end t)
       (prog1 t
         (save-excursion
           (goto-char (match-beginning 0))
           (when-let (((save-match-data (not (nth 8 (syntax-ppss)))))
                      ((fboundp 'read-positioning-symbols))
                      (expr (ignore-errors
                              (read-positioning-symbols (current-buffer)))))
             (put-text-property (match-beginning 0) (point)
                                'font-lock-multiline t)
             (llama--fontify (cdr expr) nil nil t))))))

(defun llama--fontify (expr &optional fnpos backquoted top)
  (cond
   ((null expr) expr)
   ((eq (car-safe expr) 'quote))
   ((eq (ignore-errors (bare-symbol (car-safe expr))) 'quote))
   ((and (memq (ignore-errors (bare-symbol (car-safe expr)))
               (list (intern "") 'llama))
         (not top)))
   ((and backquoted (symbol-with-pos-p expr)))
   ((and backquoted
         (memq (car-safe expr)
               (list backquote-unquote-symbol
                     backquote-splice-symbol)))
    (llama--fontify expr))
   ((symbol-with-pos-p expr)
    (save-match-data
      (when-let*
          ((name (symbol-name (bare-symbol expr)))
           (face (cond
                  ((and (string-match
                         "\\_<\\(?:\\(_\\)?%\\([1-9]\\)?\\)\\_>" name)
                        (or (not fnpos) (match-end 2)))
                   'llama-mandatory-argument)
                  ((and (string-match
                         "\\_<\\(?:\\(_\\)?&\\([1-9*]\\)?\\)\\_>" name)
                        (or (not fnpos) (match-end 2)))
                   'llama-optional-argument))))
        (when (match-end 1)
          (setq face (list 'llama-deleted-argument face)))
        (let ((beg (symbol-with-pos-pos expr)))
          (put-text-property
           beg (save-excursion (goto-char beg) (forward-symbol 1))
           'face face)))))
   ((or (listp expr)
        (vectorp expr))
    (let* ((vectorp (vectorp expr))
           (expr (if vectorp (append expr ()) expr))
           (fnpos (and (not vectorp)
                       (not backquoted)
                       (ignore-errors (length expr)))))
      (catch t
        (while t
          (cond ((eq (car expr) backquote-backquote-symbol)
                 (setq expr (cdr expr))
                 (llama--fontify (car expr) t t))
                ((llama--fontify (car expr) fnpos backquoted)))
          (setq fnpos nil)
          (setq expr (cdr expr))
          (unless (and expr
                       (listp expr)
                       (not (eq (car expr) backquote-unquote-symbol)))
            (throw t nil))))
      (when expr
        (llama--fontify expr fnpos))))))

(defvar llama-fontify-mode-lighter nil)

;;;###autoload
(define-minor-mode llama-fontify-mode
  "Toggle fontification of the `##' macro and its positional arguments."
  :lighter llama-fontify-mode-lighter
  (if llama-fontify-mode
      (font-lock-add-keywords  nil llama-font-lock-keywords)
    (font-lock-remove-keywords nil llama-font-lock-keywords)))

(defun llama--turn-on-fontify-mode ()
  "Enable `llama-fontify-mode' if in an Emacs Lisp buffer."
  (when (derived-mode-p #'emacs-lisp-mode)
    (llama-fontify-mode)))

;;;###autoload
(define-globalized-minor-mode global-llama-fontify-mode
  llama-fontify-mode llama--turn-on-fontify-mode)

(provide 'llama)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; llama.el ends here
