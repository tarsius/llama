((emacs-lisp-mode
  (indent-tabs-mode . nil)
  ;; Instead of at the bottom of each library, you can also configure
  ;; the shorthands in this file.  Note that Melpa by default does not
  ;; include it in packages, so you will have to explicitly add it to
  ;; `:files' in the Melpa recipe.  See `forge's recipe for an example.
  (read-symbol-shorthands
   ("partial" . "llama--left-apply-partially")
   ("rpartial" . "llama--right-apply-partially"))))
