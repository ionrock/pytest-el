;; pytest.el --- Easy Python test running in Emacs

;; Copyright (C) 2009 Eric Larson

;; Licensed under the same terms as Emacs.

;; Version: 0.1.0
;; Keywords: pytest python testing
;; Created: 07 Oct 2011

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:
;; This gives a bunch of functions that handle running pytest on a
;; particular buffer or part of a buffer. This started as a direct
;; port of nosemacs (https://bitbucket.org/durin42/nosemacs). A
;; special thanks to Jason Pellerin and Augie Fackler for writing
;; nose.el.

;;; Installation

;; In your emacs config:
;;
;; (require 'pytest)
;; ; next line only for people with non-eco non-global test runners
;; ; (add-to-list 'pytest-project-names "my/crazy/runner")
;; This should be a file or script that will run the tests

;; Note that if your global pytest isn't called "pytest", then you'll want to
;; redefine pytest-global-name to be the command that should be used.

;; By default, the root of a project is found by looking for any of the files
;; 'setup.py', '.hg' and '.git'. You can add files to check for to the file
;; list:
;;
;; ; (add-to-list 'pytest-project-root-files "something")

;; or you can change the project root test to detect in some other way
;; whether a directory is the project root:
;;
;; ; (setq pytest-project-root-test (lambda (dirname) (equal dirname "foo")))

;; If you want dots as output, rather than the verbose output:
;; (defvar pytest-use-verbose nil) ; default is t

;; Probably also want some keybindings:
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key "\C-ca" 'pytest-all)
;;             (local-set-key "\C-cm" 'pytest-module)
;;             (local-set-key "\C-c." 'pytest-one)
;;             (local-set-key "\C-cpa" 'pytest-pdb-all)
;;             (local-set-key "\C-cpm" 'pytest-pdb-module)
;;             (local-set-key "\C-cp." 'pytest-pdb-one)))

(defvar pytest-project-names '("runtests.py"))
(defvar pytest-project-root-files '("setup.py" ".hg" ".git"))
(defvar pytest-project-root-test 'pytest-project-root)
(defvar pytest-global-name "pytest")
(defvar pytest-use-verbose nil)

(defun run-pytest (&optional tests debug failed)
  "run pytest"
  (let* ((pytest (pytest-find-test-runner))
         (where (pytest-find-project-root))
         (args (concat (if debug "--pdb" "")
                       (if debug " " "")
                       (if failed "-f" "")))
         (tnames (if tests tests "")))
    (funcall (if debug
                 'pdb
               '(lambda (command)
                  (compilation-start command
                                     nil
                                     (lambda (mode) (concat "*pytest*")))))
             (format
              (concat "%s "
                      (if pytest-use-verbose "-v " "")
                      "%s %s")
              (pytest-find-test-runner) args tnames))))

(defun pytest-all (&optional debug failed)
  "run all tests"
  (interactive)
  (run-pytest nil debug failed))

(defun pytest-failed (&optional debug)
  (interactive)
  (pytest-all debug t))

(defun pytest-pdb-all ()
  (interactive)
  (pytest-all t))

(defun pytest-module (&optional debug)
  "run pytest (via eggs/bin/test) on current buffer"
  (interactive)
  (run-pytest buffer-file-name debug))

(defun pytest-pdb-module ()
  (interactive)
  (pytest-module t))

(defun pytest-one (&optional debug)
  "run pytest (via eggs/bin/test) on testable thing
 at point in current buffer"
  (interactive)
  (run-pytest (format "-k %s %s" (pytest-py-testable) buffer-file-name) debug))

(defun pytest-pdb-one ()
  (interactive)
  (pytest-one t))

(defun pytest-find-test-runner ()
  (message
   (let ((result
          (reduce '(lambda (x y) (or x y))
        (mapcar 'pytest-find-test-runner-names pytest-project-names))))
     (if result
         result
       pytest-global-name))))

(defun pytest-find-test-runner-names (runner)
  "find eggs/bin/test in a parent dir of current buffer's file"
  (message (concat "looking for " runner))
  (pytest-find-test-runner-in-dir-named
   (file-name-directory buffer-file-name) runner))

(defun pytest-find-test-runner-in-dir-named (dn runner)
  (let ((fn (expand-file-name runner dn)))
    (cond ((file-regular-p fn) fn)
      ((equal dn "/") nil)
      (t (pytest-find-test-runner-in-dir-named
          (file-name-directory (directory-file-name dn))
          runner)))))

(defun pytest-py-testable ()
  (let* ((inner-obj (inner-testable))
         (outer (outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (cond ((equal outer-def "def") outer-obj)
          ((equal inner-obj outer-obj) outer-obj)
          (t (format "%s.%s" outer-obj inner-obj)))))

(defun inner-testable ()
  (save-excursion
    (re-search-backward
     "^ \\{0,4\\}\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun outer-testable ()
  (save-excursion
    (re-search-backward
     "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
            (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun pytest-find-project-root (&optional dirname)
  (let ((dn
         (if dirname
             dirname
           (file-name-directory buffer-file-name))))
    (cond ((funcall pytest-project-root-test dn) (expand-file-name dn))
          ((equal (expand-file-name dn) "/") nil)
        (t (pytest-find-project-root
             (file-name-directory (directory-file-name dn)))))))

(defun pytest-project-root (dirname)
  (reduce '(lambda (x y) (or x y))
          (mapcar (lambda (d) (member d (directory-files dirname)))
                  pytest-project-root-files)))

(provide 'pytest)

;;; pytest.el ends here
