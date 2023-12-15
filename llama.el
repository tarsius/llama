;;; llama.el --- Compact syntax for short lambda  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2023 Jonas Bernoulli

;; Authors: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://git.sr.ht/~tarsius/llama
;; Keywords: extensions

;; Package-Requires: ((seq "2.23"))

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

;; This package implements the macro `##', which provides compact
;; syntax for short `lambda', without actually being new syntax,
;; which would be difficult to get merged into Emacs.  Past attempts
;; to add syntax were met with determined pushback and the use of a
;; macro was suggested as an alternative.

;; The `##' macro, whose signature is (## FN &rest ARGS), expands
;; to a `lambda' expression, which wraps around its arguments.

;; This `lambda' expression calls the function FN with arguments
;; ARGS and returns its value.  Its own arguments are derived from
;; symbols found in ARGS.

;; Each symbol from `%1' through `%9', which appears in ARGS,
;; specifies an argument.  Each symbol from `&1' through `&9', which
;; appears in ARGS, specifies an optional argument.  All arguments
;; following an optional argument have to be optional as well, thus
;; their names have to begin with `&'.  Symbol `&*' specifies extra
;; (`&rest') arguments.

;; Instead of `%1', the shorthand `%' can be used; but that should
;; only be done if it is the only argument, and using both `%1' and
;; `%' is not allowed.  Likewise `&' can be substituted for `&1'.

;; Instead of:
;;
;;   (lambda (a _ &optional c &rest d)
;;     (foo a (bar c) d))
;;
;; you can use this macro and write:
;;
;;   (##foo %1 (bar &3) &*)
;;
;; which expands to:
;;
;;   (lambda (%1 _%2 &optional &3 &rest &*)
;;     (foo %1 (bar &3) &*))

;; The name `##' was chosen because that allows (optionally)
;; omitting the whitespace between it and the following symbol.
;; It also looks similar to #'function.

;;; Code:

(require 'seq)

;;;###autoload
(defmacro ## (fn &rest args)
    "Expand to a `lambda' expression that wraps around FN and ARGS.

This `lambda' expression calls the function FN with arguments
ARGS and returns its value.  Its own arguments are derived from
symbols found in ARGS.

Each symbol from `%1' through `%9', which appears in ARGS,
specifies an argument.  Each symbol from `&1' through `&9', which
appears in ARGS, specifies an optional argument.  All arguments
following an optional argument have to be optional as well, thus
their names have to begin with `&'.  Symbol `&*' specifies extra
\(`&rest') arguments.

Instead of `%1', the shorthand `%' can be used; but that should
only be done if it is the only argument, and using both `%1' and
`%' is not allowed.  Likewise `&' can be substituted for `&1'.

Instead of:

  (lambda (a _ &optional c &rest d)
    (foo a (bar c) d))

you can use this macro and write:

  (##foo %1 (bar &3) &*)

which expands to:

  (lambda (%1 _%2 &optional &3 &rest &*)
    (foo %1 (bar &3) &*))

The name `##' was chosen because that allows (optionally)
omitting the whitespace between it and the following symbol.
It also looks a bit like #\\='function."
  (unless (symbolp fn)
    (signal 'wrong-type-argument (list 'symbolp fn)))
  `(lambda ,(llama--arguments args)
     (,fn ,@args)))

(defun llama--arguments (data)
  (let ((args (make-vector 10 nil)))
    (llama--collect data args)
    (let ((optional nil)
          (pos 0))
      (apply #'nconc
             (mapcar
              (lambda (symbol)
                (setq pos (1+ pos))
                (cond
                 ((not symbol)
                  (list (intern (format "_%s%s" (if optional "&" "%") pos))))
                 ((eq (aref (symbol-name symbol) 0) ?%)
                  (when optional
                    (error "`%s' cannot follow optional arguments" symbol))
                  (list symbol))
                 ((eq symbol '&*)
                  (list '&rest symbol))
                 (optional
                  (list symbol))
                 ((setq optional t)
                  (list '&optional symbol))))
              (vconcat (reverse (seq-drop-while
                                 #'null
                                 (reverse (seq-subseq args 1))))
                       (and-let* ((rest (aref args 0))) (list rest))))))))

(defun llama--collect (data args)
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)) pos)
      (save-match-data
        (when (string-match "\\`[%&]\\([1-9*]\\)?\\'" name)
          (setq pos (match-string 1 name))
          (setq pos (cond ((equal pos "*") 0)
                          ((not pos) 1)
                          ((string-to-number pos))))
          (when (and (= pos 1)
                     (aref args 1)
                     (not (equal data (aref args 1))))
            (error "`%s' and `%s' are mutually exclusive" data (aref args 1)))
          (aset args pos data)))))
   ((and (not (eq (car-safe data) '##))
         (or (listp data)
             (vectorp data)))
    (seq-doseq (elt data)
      (llama--collect elt args)))))

;;; Advices

(defun llama--expect-function-p (fn pos)
  (or (and (eq (char-before    pos)    ?#)
           (eq (char-before (- pos 1)) ?#))
      (and (eq (char-before    pos)    ?\s)
           (eq (char-before (- pos 1)) ?#)
           (eq (char-before (- pos 2)) ?#))
      (funcall fn pos)))

(advice-add 'elisp--expect-function-p :around #'llama--expect-function-p)

(when (eval (fboundp 'elisp-mode-syntax-propertize) t)
  ;; Synced with Emacs up to 6b9510d94f814cacf43793dce76250b5f7e6f64a.
  (defun llama--elisp-mode-syntax-propertize (start end)
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
  (advice-add 'elisp-mode-syntax-propertize :override
              'llama--elisp-mode-syntax-propertize))

;;; Fontification

(defgroup llama ()
  "Compact syntax for short lambda."
  :group 'extensions
  :group 'lisp)

(defface llama-macro '((t :inherit font-lock-function-call-face))
  "Face used for the name of the `##' macro.")

(defface llama-mandatory-argument '((t :inherit font-lock-variable-use-face))
  "Face used for mandatory arguments `%1' through `%9' and `%'.")

(defface llama-optional-argument '((t :inherit font-lock-type-face))
  "Face used for optional arguments `&1' through `&9', `&' and `&*'.")

(defvar llama-font-lock-keywords
  `(("(\\(##\\)" 1 'llama-macro)
    ("\\_<\\(?:%[1-9]?\\)\\_>" 0 'llama-mandatory-argument)
    ("\\_<\\(?:&[1-9*]\\)\\_>" 0 'llama-optional-argument)))

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
