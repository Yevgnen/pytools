;;; pytools.el ---
;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((pythonic "0.1.1"))
;; Keywords: python
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Missing Python tools.
;;
;; See documentation on https://github.com/Yevgnen/pytools.

;;; Code:

(require 'pythonic)

;;;###autoload
(defun pytools-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  (setq compile-command
        (format "%s %s" python-shell-interpreter (file-name-nondirectory buffer-file-name)))
  (setq universal-argument t)
  (if arg
      (call-interactively 'compile)
    (compile compile-command t))
  (with-current-buffer (get-buffer "*compilation*")
    (comint-mode)))

;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
;;;###autoload
(defun pytools-annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import i?pu?db")
  (highlight-lines-matching-regexp "i?pu?db.set_trace()"))

;;;###autoload
(defun pytools-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (previous-line)
  (let ((trace (cond ((executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ((executable-find "pudb") "import pudb; pudb.set_trace()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (next-line)
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line)))))

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
;;;###autoload
(defun pytools-remove-unused-imports()
  "Use Autoflake to remove unused function.

Command line: autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (user-error "autoflake executable not found")))

;;;###autoload
(defun pytools-cleanup-buffer ()
  "Just simply clean up a python buffer."
  (interactive)
  (delete-trailing-whitespace)
  (untabify-buffer))

;; Virtual environment support
(make-local-variable 'python-shell-virtualenv-root)

;;;###autoload
(defun pytools-pyenv-name (&optional fullname)
  (if-let* ((env-file (locate-dominating-file default-directory ".python-version"))
            (env (with-temp-buffer
                   (insert-file-contents
                    (expand-file-name ".python-version" env-file))
                   (car (split-string (buffer-string) nil t)))))
      (if fullname
          (format "%s/versions/%s" (getenv "PYENV_ROOT") env)
        env)))

;;;###autoload
(defun pytools-pyenv-set-env ()
  (when-let* ((env (pytools-pyenv-name))
              (env-root (getenv "PYENV_ROOT")))
    (pythonic-activate (format "%s/versions/%s" env-root env))
    (setq mode-name (format "%s[%s]" mode-name env))))

(provide 'pytools)

;;; pytools.el ends here
