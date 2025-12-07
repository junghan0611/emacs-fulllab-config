;;; my-org-literate.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: February 20, 2025
;; Modified: February 20, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/junghan0611/dotemacs
;; Package-Requires: ((emacs "29.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;; #+property: header-args:jupyter-python :session python :tangle python.py

;;; Code:


;;; ha-org-literate

(defun ha-org-block-language ()
  "Return the language associated with the current block."
  (save-excursion
    (re-search-backward (rx bol (zero-or-more space)
                            "#+begin_src"
                            (one-or-more space)
                            (group (one-or-more (not space)))))
    (match-string 1)))

(defun ha-org-block-language-and ()
  "Return language and parameters of the current block."
  (save-excursion
    (re-search-backward (rx bol (zero-or-more space)
                            "#+begin_src"
                            (one-or-more space)
                            (group (one-or-more any))))
    (match-string 1)))

;;;; Visit Tangled File

;; After tangling a file, you might want to take a look at it. But a single org file can tangle out to more than one file, so how to choose? Each block could have its one file, as would the particular language-specific blocks in a section or the entire file. I don’t care to be too pendantic here, because I often know the name of the file.

;; Perhaps a little function to map a /language/ to a file name:

(defun ha-org-babel-tangle-default (language)
  "Return the default tangled file based on LANGUAGE."
  (format "%s.%s"
          (file-name-sans-extension (buffer-file-name))
          (pcase language
            ("emacs-lisp" "el")
            ("elisp" "el")
            ("python" "py")
            ("json" "json")
            ("yaml" "yml")
            ("jupyter-python" "py")
            ;; ("ruby" "rb")
            ;; ...
            ("bash" "sh")
            ("sh" "sh")
            ("shell" "sh"))))

;; This function returns the name of the tangled file based on either the block or a default name based on the file name:

(defun ha-org-babel-tangle-file-name ()
  "Return _potential_ file name or Org source.
  If nearest block has a `:tangle' entry, return that.
  If block doesn't have `:tangle' (or set to `yes'),
  return the filename based on the block's language."
  (save-excursion
    (org-previous-block 1)
    (when (looking-at
           (rx "#+begin_src"
               (one-or-more space)
               (group (one-or-more (not space)))
               (one-or-more space) (zero-or-more any)
               (optional
                (group ":tangle" (one-or-more space)
                       (optional "\"")
                       (group (one-or-more any))
                       (optional "\"")))))
      (let ((language (match-string 1))
            (filename (match-string 3)))
        (if (or (null filename) (equal filename "yes") (equal filename "true"))
            (ha-org-babel-tangle-default language)
          filename)))))

;; And this function calls =find-file= on the tangled file (if found):

(defun ha-org-babel-tangle-visit-file ()
  "Attempt to call `find-file' on Org's tangled file."
  (interactive)
  (let ((tangled-file (ha-org-babel-tangle-file-name)))
    (if (file-exists-p tangled-file)
        (find-file tangled-file)
      (message "Looking for %s, which doesn't exist." tangled-file))))

;;;;; :comments link 활용

;; *Note:* You can /return/ to the original Org file with =org-babel-tangle-jump-to-org=, if you set the tangle =comments= to =link=, as in this global property:
;; ,#+PROPERTY: header-args:emacs-lisp :tangle yes :comments link

;;;; Navigating Code Blocks

;; I’ve been using Oleh Krehel’s (abo-abo) [[https://github.com/abo-abo/avy][Avy project]] to jump around the screen for years, and I just learned that I can wrap the =avy-jump= function to provide either/or regular expression and action to perform.

;; For instance, the following function can be used to quickly select a source code block, and jump to it:

(require 'avy)
(defun avy-jump-org-block ()
  "Jump to org block using Avy subsystem."
  (interactive)
  (avy-jump (rx line-start (zero-or-more blank) "#+begin_src")
            :action 'goto-char)
  ;; Jump _into_ the block:
  (forward-line))

;; I need to take advantage of this feature more.

;;;; Evaluating Code

;; Hitting ~C-c C-c~ in a source code block /evaluates/ the code. Simple, sure, but the following enhancements make this more accessible.

(defun org-babel-execute-src-block-at-point (&optional point)
  "Call `org-babel-execute-src-block' at POINT."
  (save-excursion
    (goto-char point)
    (org-babel-execute-src-block)))

(defun avy-org-babel-execute-src-block ()
  "Call `org-babel-execute-src-block' on block given by Avy.
  Use Avy subsystem to select a visible Org source code block,
  e.g. `#+begin_src', and then executes the code without moving
  the point."
  (interactive)
  (avy-jump (rx line-start (zero-or-more blank) "#+begin_src")
            :action 'org-babel-execute-src-block-at-point))

(defun org-babel-execute-subtree (prefix)
  "Execute all Org source blocks in current subtree.
  The PREFIX is passed to `org-babel-execute-buffer'."
  (interactive "P")
  (save-excursion
    (org-narrow-to-subtree)
    (org-babel-execute-buffer prefix)
    (widen)))

(defun org-babel-edit-src-block-at-point (&optional point)
  "Call `org-babel-execute-src-block' at POINT."
  (save-excursion
    (goto-char point)
    (org-edit-src-code)))

(defun avy-org-babel-edit-src-block ()
  "Call `org-edit-src-code' on block given by Avy.
  Use Avy subsystem to select a visible Org source code block,
  e.g. `#+begin_src', and then executes the code without moving
  the point."
  (interactive)
  (avy-jump (rx line-start (zero-or-more blank) "#+begin_src")
            :action
            'org-babel-edit-src-block-at-point))

;;;; TODO Finding Code


;;;; Keybindings

(when (locate-library "pretty-hydra")
  (require 'pretty-hydra)
  (pretty-hydra-define hydra-org-babel
    (:color amaranth :quit-key "<escape>")
    (
     "Code Blocks"
     (("g" avy-jump-org-block "avy Jump")
      ("j" org-next-block "Previous" :color pink)
      ("k" org-previous-block "Next" :color pink))
     "Evaluate"
     (("o" avy-org-babel-execute-src-block "avy Block")
      ("h" org-babel-execute-subtree "Section")
      ("b" org-babel-execute-buffer "Buffer"))
     "Tangle"
     (("t" org-babel-tangle "to Default")
      ("f" org-babel-tangle-file "choose File")
      ("T" org-babel-detangle "from File"))
     ;; "AI"
     ;; (("s" gpt-babel/send-block)
     ;;  ("p" gpt-babel/patch-block))
     "Misc"
     (("e" avy-org-babel-edit-src-block "avy Edit Block")
      ("s" org-babel-pop-to-session-maybe "Session REPL")
      ("v" ha-org-babel-tangle-visit-file "Visit Tangled")
      ("q" nil "Quit" :color red :exit t)
      )
     )
    )
  (keymap-set org-mode-map "C-c 0" #'hydra-org-babel/body)
  (keymap-set org-mode-map "<f5>" #'hydra-org-babel/body)
  )

;;; provide

(provide 'my-org-literate)

;;; my-literate.el ends here
