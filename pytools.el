;;; pytools.el ---
;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
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


(defcustom pytools-defualt-debugger "ipdb"
  "Defualt debugger")

;; From: https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/python/funcs.el
;;;###autoload
(defun pytools-pyenv-executable-find (command)
  "Find executable taking pyenv shims into account."
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command))))
          (unless (string-match "not found" pyenv-string)
            pyenv-string)))
    (executable-find command)))

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
(defun pytools-toggle-breakpoint (arg)
  "Add a break point, highlight it."
  (interactive "P")
  (previous-line)
  (let ((trace (cond ((and arg (pytools-pyenv-executable-find "ipdb3")) "import ipdb; ipdb.set_trace()")
                     ((and arg (pytools-pyenv-executable-find "pudb3")) "import pudb; pudb.set_trace()")
                     ((and arg (pytools-pyenv-executable-find "ipdb")) "import ipdb; ipdb.set_trace()")
                     ((and arg (pytools-pyenv-executable-find "pudb")) "import pudb; pudb.set_trace()")
                     (t (format "import %s; %s.set_trace()" pytools-defualt-debugger pytools-defualt-debugger))))
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
(defun pytools-remove-unused-imports(&optional isort)
  "Use Autoflake to remove unused function.

Command line: autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t)
        (if (and isort
                 (require 'py-isort nil t))
            (py-isort-buffer)))
    (user-error "autoflake executable not found")))

;;;###autoload
(defun pytools-cleanup-buffer ()
  "Just simply clean up a python buffer."
  (interactive)
  (delete-trailing-whitespace)
  (untabify-buffer))

;;;###autoload
(defun pytools-setup-yapf-git-hook ()
  (interactive)
  (when-let ((root (locate-dominating-file default-directory ".git"))
             (cmd (string-join
                   `(,(format "cd %s" root)
                     "curl -o pre-commit.sh https://raw.githubusercontent.com/google/yapf/master/plugins/pre-commit.sh"
                     "chmod a+x pre-commit.sh"
                     "mv pre-commit.sh .git/hooks/pre-commit")
                   ";")))
    (call-process-shell-command cmd nil "*Shell Command Output*" t)))

(provide 'pytools)

;;; pytools.el ends here
