;;; llama-tests.el --- Tests for Llama  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2025 Jonas Bernoulli

;; Authors: Jonas Bernoulli <emacs.llama@jonas.bernoulli.dev>
;; Homepage: https://github.com/tarsius/llama
;; Keywords: extensions

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

;;; Code:

(require 'llama)

(ert-deftest llama-test-101-basic nil

  (should (equal (##list  %1)
                 (lambda (%1)
                   (list  %1))))

  (should (equal (##list  %1 %1)
                 (lambda (%1)
                   (list  %1 %1))))

  (should (equal (##list  %1 %2)
                 (lambda (%1 %2)
                   (list  %1 %2))))

  (should (equal (##list     %2 %1)
                 (lambda (%1 %2)
                   (list     %2 %1))))

  (should (equal (##list  'const %1)
                 (lambda (       %1)
                   (list  'const %1))))

  (should (equal (##list  %1 'const)
                 (lambda (%1)
                   (list  %1 'const))))

  (should (equal (##list  %1 'const %2)
                 (lambda (%1        %2)
                   (list  %1 'const %2))))

  (should (equal (##list     %2 'const %1)
                 (lambda (%1 %2)
                   (list     %2 'const %1))))

  (should (equal (##list  %1 %2 %3 %4 %5 %6 %7 %8 %9)
                 (lambda (%1 %2 %3 %4 %5 %6 %7 %8 %9)
                   (list  %1 %2 %3 %4 %5 %6 %7 %8 %9))))

  (should (equal (##list  %1 %2 %1 %3 %5 %4    %6 %7    %9 %8)
                 (lambda (%1 %2    %3    %4 %5 %6 %7 %8 %9)
                   (list  %1 %2 %1 %3 %5 %4    %6 %7    %9 %8))))
  )

(ert-deftest llama-test-102-basic-optional nil

  (should (equal (##list            &1)
                 (lambda (&optional &1)
                   (list            &1))))

  (should (equal (##list  %1           &2)
                 (lambda (%1 &optional &2)
                   (list  %1           &2))))

  (should (equal (##list  %2 %1                 &4 &3)
                 (lambda (   %1 %2 &optional &3 &4)
                   (list  %2 %1                 &4 &3))))
  )

(ert-deftest llama-test-103-basic-rest nil

  (should (equal (##list        &*)
                 (lambda (&rest &*)
                   (list        &*))))

  (should (equal (##list  %1       &*)
                 (lambda (%1 &rest &*)
                   (list  %1       &*))))

  (should (equal (##list  %1           &2       &*)
                 (lambda (%1 &optional &2 &rest &*)
                   (list  %1           &2       &*))))
  )

(ert-deftest llama-test-104-basic-nested nil

  (should (equal (##list (##list %) %1)
                 (lambda (%1)
                   (list (lambda (%) (list %))
                         %1))))
  )

(ert-deftest llama-test-105-basic-nil nil

  (should (equal (##list (##list %) %1)
                 (lambda (%1)
                   (list (lambda (%) (list %))
                         %1))))
  )

(ert-deftest llama-test-201-unused-implicit-mandatory nil

  (should (equal (##list      %2)
                 (lambda (_%1 %2)
                   (list      %2))))

  (should (equal (##list      %2 %3)
                 (lambda (_%1 %2 %3)
                   (list      %2 %3))))

  (should (equal (##list          %3)
                 (lambda (_%1 _%2 %3)
                   (list          %3))))

  (should (equal (##list  %1     %3)
                 (lambda (%1 _%2 %3)
                   (list  %1     %3))))

  (should (equal (##list          %3         %6)
                 (lambda (_%1 _%2 %3 _%4 _%5 %6)
                   (list          %3         %6))))
  )

(ert-deftest llama-test-202-unused-implicit-optional nil

  (should (equal (##list                &2)
                 (lambda (&optional _&1 &2)
                   (list                &2))))

  (should (equal (##list                &2 &3)
                 (lambda (&optional _&1 &2 &3)
                   (list                &2 &3))))

  (should (equal (##list                    &3)
                 (lambda (&optional _&1 _&2 &3)
                   (list                    &3))))

  (should (equal (##list            &1     &3)
                 (lambda (&optional &1 _&2 &3)
                   (list            &1     &3))))

  (should (equal (##list                    &3         &6)
                 (lambda (&optional _&1 _&2 &3 _&4 _&5 &6)
                   (list                    &3         &6))))
  )

(ert-deftest llama-test-203-unused-implicit-mixed nil

  (should (equal (##list  %1               &3)
                 (lambda (%1 &optional _&2 &3)
                   (list  %1               &3))))

  (should (equal (##list  %1                   &4)
                 (lambda (%1 &optional _&2 _&3 &4)
                   (list  %1                   &4))))

  (should (equal (##list  %1 %2               &4)
                 (lambda (%1 %2 &optional _&3 &4)
                   (list  %1 %2               &4))))


  (should (equal (##list      %2               &4     &6)
                 (lambda (_%1 %2 &optional _&3 &4 _&5 &6)
                   (list      %2               &4     &6))))
  )

(ert-deftest llama-test-301-unused-explicit-trailing nil

  (should (equal (##list  _%1)
                 (lambda (_%1)
                   (list))))

  (should (equal (##list      _%2)
                 (lambda (_%1 _%2)
                   (list))))

  (should (equal (##list  %1 _%2)
                 (lambda (%1 _%2)
                   (list  %1))))

  (should (equal (##list  %1     _%3)
                 (lambda (%1 _%2 _%3)
                   (list  %1))))
  )

(ert-deftest llama-test-302-unused-explicit-border nil

  (should (equal (##list  _%1           &2)
                 (lambda (_%1 &optional &2)
                   (list                &2))))

  (should (equal (##list      _%2           &3)
                 (lambda (_%1 _%2 &optional &3)
                   (list                    &3))))

  (should (equal (##list  %1 _%2           &3)
                 (lambda (%1 _%2 &optional &3)
                   (list  %1               &3))))

  (should (equal (##list  %1 _%2               &4)
                 (lambda (%1 _%2 &optional _&3 &4)
                   (list  %1                   &4))))

  (should (equal (##list  %1     _%3                   &6)
                 (lambda (%1 _%2 _%3 &optional _&4 _&5 &6)
                   (list  %1                           &6))))
  )

(ert-deftest llama-test-303-unused-redundant nil

  (should (equal (##list  _%1 %2)
                 (lambda (_%1 %2)
                   (list      %2))))

  (should (equal (##list            _&1 &2)
                 (lambda (&optional _&1 &2)
                   (list                &2))))
  )

(ert-deftest llama-test-401-abbrev nil
  ;; llama-test-101-basic(s/%1/%/)

  (should (equal (##list  %)
                 (lambda (%)
                   (list  %))))

  (should (equal (##list  % %)
                 (lambda (%)
                   (list  % %))))

  (should (equal (##list  % %2)
                 (lambda (% %2)
                   (list  % %2))))

  (should (equal (##list     %2 %)
                 (lambda (% %2)
                   (list     %2 %))))

  (should (equal (##list  'const %)
                 (lambda (       %)
                   (list  'const %))))

  (should (equal (##list  % 'const)
                 (lambda (%)
                   (list  % 'const))))

  (should (equal (##list  % 'const %2)
                 (lambda (%        %2)
                   (list  % 'const %2))))

  (should (equal (##list     %2 'const %)
                 (lambda (% %2)
                   (list     %2 'const %))))

  (should (equal (##list  % %2 %3 %4 %5 %6 %7 %8 %9)
                 (lambda (% %2 %3 %4 %5 %6 %7 %8 %9)
                   (list  % %2 %3 %4 %5 %6 %7 %8 %9))))

  (should (equal (##list  % %2 % %3 %5 %4    %6 %7    %9 %8)
                 (lambda (% %2   %3    %4 %5 %6 %7 %8 %9)
                   (list  % %2 % %3 %5 %4    %6 %7    %9 %8))))
  )

(ert-deftest llama-test-402-abbrev-optional nil
  ;; llama-test-102-basic-optional(s/&1/&/)

  (should (equal (##list            &1)
                 (lambda (&optional &1)
                   (list            &1))))

  (should (equal (##list  %           &2)
                 (lambda (% &optional &2)
                   (list  %           &2))))

  (should (equal (##list  %2 %                 &4 &3)
                 (lambda (   % %2 &optional &3 &4)
                   (list  %2 %                 &4 &3))))
  )

(ert-deftest llama-test-501-function-position nil

  (should (equal (##+ (% %2 2) %1)
                 (lambda (%1 %2)
                   (+ (% %2 2) %1))))

  (should (equal (##+ (* %2 2) %)
                 (lambda (% %2)
                   (+ (* %2 2) %))))

  (should (equal (##% %2 2)
                 (lambda (_%1 %2)
                   (% %2 2))))

  (should (equal (##* %1 2)
                 (lambda (%1)
                   (* %1 2))))

  (should (equal (##% %2 %1)
                 (lambda (%1 %2)
                   (% %2 %1))))
  )

(defmacro llama-test--flatten (expr)
  (when (vectorp expr)
    (setq expr (mapcan (lambda (e)
                        (if (vectorp e) (append e ()) (list e)))
                      (append expr ()))))
  (let ((body ()))
    (while expr
      (if (listp expr) (push (pop expr) body) (push expr body) (setq expr nil)))
    (cons 'list (nreverse body))))

(ert-deftest llama-test-502-vector nil

  ;; Real world example: (##-let [val %1] ...).

  (should (equal (##llama-test--flatten [[1 %1]])
                 (lambda (%1)
                   (llama-test--flatten [[1 %1]]))))

  (should (equal (##llama-test--flatten [%2 [%1]])
                 (lambda (%1 %2)
                   (llama-test--flatten [%2 [%1]]))))

  (should (equal (##llama-test--flatten [%1 _%2 %3])
                 (lambda (%1 _%2 %3)
                   (llama-test--flatten [%1 %3]))))
  )

(ert-deftest llama-test-502-dotted nil

  ;; Real world example: ???.

  (should (equal (##llama-test--flatten (%1 . %2))
                 (lambda (%1 %2)
                   (llama-test--flatten (%1 . %2)))))

  (should (equal (##llama-test--flatten (%1 %2 . %3))
                 (lambda (%1 %2 %3)
                   (llama-test--flatten (%1 %2 . %3)))))

  (should (equal (##llama-test--flatten (%1 _%2 . %3))
                 (lambda (%1 _%2 %3)
                   (llama-test--flatten (%1 . %3)))))

  (should (equal (##llama-test--flatten (%1 _%2 %3 . %4))
                 (lambda (%1 _%2 %3 %4)
                   (llama-test--flatten (%1 %3 . %4)))))
  )

(ert-deftest llama-test-503-quoted nil

  (should (equal (##cons %1 '(%2))
                 (lambda (%1)
                   (cons %1 '(%2)))))
  )

(ert-deftest llama-test-504-backquoted nil

  (should (equal (##list `(,%1 %2 ,%3))
                 (lambda (%1 _%2 %3)
                   (list `(,%1 %2 ,%3)))))

  (should (equal (##list `(,%1 %2 (,%3) ,%4 . ,%5))
                 (lambda (%1 _%2 %3 %4 %5)
                   (list `(,%1 %2 (,%3) ,%4 . ,%5)))))

  (should (equal (##`(,%1 %2 ,%3))
                 (lambda (%1 _%2 %3)
                   `(,%1 %2 ,%3))))

  (should (equal (##`(,%1 %2 (,%3) ,%4 . ,%5))
                 (lambda (%1 _%2 %3 %4 %5)
                   `(,%1 %2 (,%3) ,%4 . ,%5))))

  (should (equal (##list `(,% ,@% %))
                 (lambda (%)
                   (list `(,% ,@% %)))))

  (should (equal (##list `(% ,%2))
                 (lambda (_%1 %2)
                   (list `(% ,%2)))))

  (should (equal (##list `(,@%1 %2 ,%3 (,@%3 ,%1)))
                 (lambda (%1 _%2 %3)
                   (list `(,@%1 %2 ,%3 (,@%3 ,%1))))))
  )

(ert-deftest llama-test-701-llama nil

  (should (equal (llama list %1)
                 (lambda (%1)
                   (list  %1))))

  (should (equal (llama list %1 %1)
                 (lambda (%1)
                   (list  %1 %1))))

  (should (equal (llama list %1 %2)
                 (lambda (%1 %2)
                   (list  %1 %2))))

  (should (equal (llama list %1 (llama list %))
                 (lambda (%1)
                   (list %1 (lambda (%) (list %))))))
  )

(ert-deftest llama-test-901-errors-first nil
  (should-error (##list  %1   &1))
  (should-error (##list  &1   %1))
  (should-error (##list  %1  _%1))
  (should-error (##list _%1   %1))
  (should-error (##list  %1  _&1))
  (should-error (##list _&1   %1))
  (should-error (##list  %1   %1  &1))
  )

(ert-deftest llama-test-801-ambiguity nil

  ;; We cannot know how every special form and macro uses its arguments,
  ;; and can therefore not always do the right thing™.  However, whatever
  ;; we end up doing, font-lock should agree.  Here are some noteworthy
  ;; examples where our macro expansion and our font-lock agree, but the
  ;; author might have intended something else.

  (static-if (>= emacs-major-version 28) ; prevent compiler warnings
      (with-no-warnings ; unused arguments
        ;; A good example of what we might not want and theoretically could
        ;; prevent.  However, this can also be prevented by just not going
        ;; out of our way to wander into ambiguous territory.  While not
        ;; impossible, it is unlikely that someone does this accidentally.
        (should (equal (##setq % 1)
                       (lambda (%)
                         (setq % 1))))

        ;; We have to fake `-setq' because we don't want to depend on `dash'
        ;; and because (equal (lambda () (-setq a 1)) (lambda () (-setq a 1)))
        ;; is never true because `-setq' uses `make-symbol'.  Mocking that
        ;; macro does *not* affect the expansion of `##' into a `lambda'.
        (cl-macrolet ((-setq (&rest args) `'(,@args)))
          (should (equal (##-setq % 1)
                         (lambda (%)
                           (-setq % 1))))
          (should (equal (##-setq (%) '(1))
                         (lambda ()
                           (-setq (%) '(1)))))
          (should (equal (##-setq [(%)] [(1)])
                         (lambda ()
                           (-setq [(%)] [(1)]))))
          (should (equal (##-setq [(% %)] [(1 2)])
                         (lambda (%)
                           (-setq [(% %)] [(1 2)]))))
          (should (equal (##-setq [(%1)] [(1)])
                         (lambda (%1)
                           (-setq [(%1)] [(1)]))))))
    ))

(ert-deftest llama-test-902-errors-second nil
  (should-error (##list  %2   &2))
  (should-error (##list  &2   %2))
  (should-error (##list  %2  _%2))
  (should-error (##list _%2   %2))
  (should-error (##list  %2  _&2))
  (should-error (##list _&2   %2))
  (should-error (##list  %2   %2  &2))
  )

(ert-deftest llama-test-903-errors-abbrev nil
  (should-error (##list  %    &))
  (should-error (##list  &    %))
  (should-error (##list  %   _%))
  (should-error (##list _%    %))
  (should-error (##list  %   _&))
  (should-error (##list _&    %))
  (should-error (##list  %    %   &))
  (should-error (##list  %    %1))
  (should-error (##list  %   _%1))
  (should-error (##list  %    &1))
  (should-error (##list  %   _&1))
  (should-error (##list  %1   %))
  )

(ert-deftest llama-test-904-errors-syntax nil

  ;; ((lambda (%) (+ 1 %)) 2)
  ;;   results in
  ;; Warning: Use of deprecated ((lambda (%) ...) ...) form
  ;;   but works.

  ;; ((##+ 1 %) 2)
  ;;   results at compile-time in
  ;; Warning: Malformed function ‘(## + 1 %)’
  ;;   results at run-time in
  ;; Error: invalid-function ((## + 1 %))
  ;;   and cannot possibly work.

  ;; Delay macro-expansion for demonstration purposes.
  (should-error (eval '((##+ 1 %) 2)))

  ;; This is what one should be doing instead.
  (should (equal (funcall (lambda (%) (+ 1 %)) 2) 3))
  (should (equal (funcall (##          + 1 %)  2) 3))
  )

;; Local Variables:
;; eval: (prettify-symbols-mode -1)
;; indent-tabs-mode: nil
;; End:
;;; llama-tests.el ends here
