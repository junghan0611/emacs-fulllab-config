;;; lisp/denote-funcs.el -*- lexical-binding: t; -*-

;; 파일 마이그레이션


;;;; Dired integration
;;;;; 13.5. Use =dired-virtual-mode= for arbitrary file listings

(defcustom prot-eshell-output-buffer "*Exported Eshell output*"
  "Name of buffer with the last output of Eshell command.
Used by `prot-eshell-export'."
  :type 'string
  :group 'eshell)

(defcustom prot-eshell-output-delimiter "* * *"
  "Delimiter for successive `prot-eshell-export' outputs.
This is formatted internally to have newline characters before
and after it."
  :type 'string
  :group 'eshell)

(defun prot-eshell--command-prompt-output ()
  "Capture last command prompt and its output."
  (let ((beg (save-excursion
               (goto-char (eshell-beginning-of-input))
               (goto-char (point-at-bol)))))
    (when (derived-mode-p 'eshell-mode)
      (buffer-substring-no-properties beg (eshell-end-of-output)))))

;;;;###autoload
(defun prot-eshell-export ()
  "Produce a buffer with output of the last Eshell command.
If `prot-eshell-output-buffer' does not exist, create it.  Else
append to it, while separating multiple outputs with
`prot-eshell-output-delimiter'."
  (interactive)
  (let ((eshell-output (prot-eshell--command-prompt-output)))
    (with-current-buffer (get-buffer-create prot-eshell-output-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (eq (point-min) (point-max))
          (insert (format "\n%s\n\n" prot-eshell-output-delimiter)))
        (goto-char (point-at-bol))
        (insert eshell-output)
        (switch-to-buffer-other-window (current-buffer))))))

;;;;; my/denote-link-dired-marked-notes

;; dired denote integration
;; (setq denote-link--prepare-links-format "- %s\n")
;;;;###autoload
(defun my/denote-link-dired-marked-notes (files buffer &optional ID-ONLY)
  (interactive
   (list
    (denote-link--map-over-notes)
    (let ((file-names (denote--buffer-file-names)))
      (find-file
       (cond
        ((null file-names)
         (user-error "No buffers visiting Denote notes"))
        ((eq (length file-names) 1)
         (car file-names))
        (t
         (denote-link--buffer-prompt file-names)))))
    current-prefix-arg)
   dired-mode)
  (when (null files)
    (user-error "No note files to link to"))
  (with-current-buffer buffer
    (unless (or (denote--file-type-org-extra-p)
                (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
      (user-error "The buffer's file type is not recognized by Denote")))
  (when (y-or-n-p (format "Create links at point in %s?" buffer))
    (with-current-buffer buffer
      ;; (message (format "Create links at point in %s?" buffer))
      ;; (message (format "Create links at point in \n - %s?" files))
      (insert "\n")
      (denote-link--insert-links files denote-file-type)
      (insert "\n")
      )))

;;;;; my/open-dired-denote-directory

(defun my/open-dired-denote-directory ()
  "Open dired at denote-directory"
  (interactive)
  (dired denote-directory))

;;;; Header
;;;;; customize header

(defun mho-insert-denote-identifier ()
  "Get the file's creation date and time and u se it to create a denote identifier."
  (interactive)
  (insert (format-time-string "#+identifier: %Y%m%dT%H%M%S" (nth 5 (file-attributes buffer-file-name)))))

(defun mho-insert-org-date ()
  "Get the file's creation date and time and use it to insert the date using an org format."
  (interactive)
  (insert (format-time-string "#+date: [%Y-%m-%d %a %H:%M]" (nth 5 (file-attributes buffer-file-name)))))
;;;;; org-create-id-by-denote-identifier at-once

(defun my/denote-identifier-retrieve ()
  (let* ((file (or (buffer-file-name) (dired-get-filename))))
    (when file
      (denote-retrieve-filename-identifier file))))

(defun my/org-create-id-by-denote-identifier()
  (interactive)
  (org-entry-put nil "ID" (my/denote-identifier-retrieve)))

(defun my/org-create-id-by-denote-identifier-at-once (directory extension)
  (interactive (list (read-directory-name "Directory: ")
                     (read-string "File extension: ")))
  (dolist (file (directory-files-recursively directory extension))
    (find-file file)
    (my/org-create-id-by-denote-identifier)
    (save-buffer)
    (kill-buffer nil)))

;;;;; my/org-find-time-file-property

;; https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L194
(defun my/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

;;;;; org-id-headlines : custom_id

;; https://writequit.org/articles/emacs-org-mode-generate-ids.html

;; Then we can define our own version of org-custom-id-get that calls org-id-new and creates a new property if one doesn't already exist
;; 그런 다음 org-id-new 을 호출하고 프로퍼티가 없는 경우 새 프로퍼티를 생성하는 org-custom-id-get 의 자체 버전을 정의할 수 있습니다.
;; (require 'org-id)
;; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Copied from this article (with minor tweaks from my side):
;; <https://writequit.org/articles/emacs-org-mode-generate-ids.html>.
(defun prot-org--id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present already.
PREFIX will be passed through to `org-id-new'.  In any case, the
CUSTOM_ID of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (format "%s" (buffer-file-name (buffer-base-buffer))))
        id)))))

;; And add a helper function that's interactive to add custom ids to all headlines in the buffer if they don't already have one.
;; 또한 버퍼의 모든 헤드라인에 사용자 지정 아이디가 없는 경우 대화형 도우미 기능을 추가하여 사용자 지정 아이디를 추가할 수 있습니다.

;;;;###autoload
(defun prot-org-id-headlines ()
  "Add missing CUSTOM_ID to all headlines in current file."
  (interactive)
  (org-map-entries
   (lambda () (prot-org--id-get (point) t))))

;;;;###autoload
(defun prot-org-id-headline ()
  "Add missing CUSTOM_ID to headline at point."
  (interactive)
  (prot-org--id-get (point) t))

;;;;###autoload
(defun prot-org-ox-html ()
  "Streamline HTML export."
  (interactive)
  (org-html-export-as-html nil nil nil t nil))

;;;;###autoload
(defun prot-org-ox-texinfo ()
  "Streamline Info export."
  (interactive)
  (org-texinfo-export-to-info))

;;;; Sort

(progn
  ;; (denote-explore-random-note)

  (defun denote-random-notes ()
    (interactive)
    (my/denote-random-note-from-directory (concat denote-directory "notes"))
    )

  (defun denote-random-bib ()
    (interactive)
    (my/denote-random-note-from-directory (concat denote-directory "bib"))
    )

  (defun denote-random-meta ()
    (interactive)
    (my/denote-random-note-from-directory (concat denote-directory "meta"))
    )

  )

;;;; DONT Notused
;;;;; my/org-set-time-file-property

;; (defun my/org-set-time-file-property (property &optional anywhere pos)
;;   "Set the time file PROPERTY in the preamble.
;; When ANYWHERE is non-nil, search beyond the preamble.
;; If the position of the file PROPERTY has already been computed,
;; it can be passed in POS."

;;   (when-let ((pos (or pos
;;                       (my/org-find-time-file-property property))))
;;     (save-excursion
;;       (goto-char pos)
;;       (if (looking-at-p " ")
;;           (forward-char)
;;         (insert " "))
;;       (delete-region (point) (line-end-position))
;;       (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
;;         (insert now)))))

;;;;; my/org-set-date

;; https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L212
;; (defun my/org-set-date ()
;;   "Update the LAST_MODIFIED file property in the preamble."
;;   (when (and (derived-mode-p 'org-mode)
;;              (buffer-modified-p))
;;     (my/org-set-time-file-property "DATE")))

;; (add-hook 'before-save-hook 'my/org-set-date)

;;;;; get my/reading-list

;; Get reading list from books directory for org-clock report.
;; The org-clock report scope can be a function.
;; (defun my/reading-list ()
;;   "Get reading list."
;;   (let (reading-list)
;;     (append reading-list
;;             (file-expand-wildcards (expand-file-name "bib/*.org" org-directory)))))

;;;;; mho/ denote functions

;;   (defun +denote-open-note-directory ()
;;     (interactive)
;;     (dired denote-directory))

;; (defun +denote-find-note-file ()
;;   (interactive)
;;   (let ((default-directory denote-directory))
;;     (call-interactively #'+default/find-file-under-here)))


;; (defun denote-subdirectory-new ()
;;   "Creates sub directory in the `denote-directory' for better organization"
;;   (interactive)
;;   (if-let (sd (read-string "Subdir name: " nil))
;;       (let ((subdir (file-name-concat denote-directory sd)))
;;         (if (f-dir? subdir)
;;             (message (concat "directory " subdir " already exists!"))
;;           (make-directory subdir))
;;         (denote-subdirectory subdir (denote--title-prompt) (denote--keywords-prompt)))))

;; (defun denote-browse ()
;;   "Browse files from `denote-directory'"
;;   (interactive)
;;   (unless (bound-and-true-p denote-directory)
;;     (message "denote-directoy not defined"))
;;   (doom-project-browse (concat denote-directory "/")))

;; (defun denote-subdirectory-with-date ()
;;   "Like `denote-subdirectory' but ask for date of the note."
;;   (interactive)
;;   (let ((denote-prompts '(title keywords date subdirectory)))
;;     (call-interactively #'denote)))

;;;;; DONT autocalc-clocktable

;; https://200ok.ch/posts/2022-12-07_streamline_your_org_mode_workflow_with_automatic_clock_table_recalculation.html
;; Need add #+AUTOCALC_CLOCK_TABLES to org file.
;; (with-eval-after-load 'org
;;   (add-to-list 'org-options-keywords "AUTOCALC_CLOCK_TABLES:"))

;; (defun autocalc-clocktable ()
;;   "Auto update clock table."
;;   (when (derived-mode-p 'org-mode)
;;     (save-excursion
;;       (goto-char 0)
;;       (if (string-equal (car
;;                          (cdr
;;                           (car
;;                            (org-collect-keywords '("AUTOCALC_CLOCK_TABLES")))))
;;                         "t")
;;           (progn
;;             (goto-char (search-forward "clocktable"))
;;             (org-ctrl-c-ctrl-c))))))

;; (add-hook 'before-save-hook 'autocalc-clocktable)

;;;;; DONT Rename 'screenshot': rename-all-files-to-denote-id

;; (progn
;;   (defun denote-id-p (file-name)
;;     "FILE-NAME이 이미 denote ID 형식(YYYYMMDDTHHMMSS)인지 확인."
;;     (string-match-p "\\`[0-9]\\{8\\}T[0-9]\\{6\\}\\'" (file-name-base file-name)))

;;   (defun generate-unique-file-name (directory file-name ext)
;;     "DIRECTORY 내에서 FILE-NAME과 동일한 이름이 있으면 숫자를 증가시켜 고유한 파일명 생성."
;;     (let ((counter 1)
;;           (new-file-name (concat file-name "." ext)))
;;       ;; 파일이 존재하는지 확인하고, 존재하면 숫자를 증가
;;       (while (file-exists-p (expand-file-name new-file-name directory))
;;         (setq new-file-name (format "%s-%d.%s" file-name counter ext))
;;         (setq counter (1+ counter)))
;;       new-file-name))

;;   (defun rename-to-denote-id (file-name)
;;     "FILE-NAME에서 날짜와 시간을 추출해 denote 형식으로 파일명을 변경.
;; denote ID 형식의 파일명은 건너뜀. 중복 파일명이 있으면 숫자를 증가시킴."
;;     (let* ((base-name (file-name-base file-name))
;;            (ext (file-name-extension file-name))
;;            (directory (file-name-directory file-name))
;;            ;; 날짜와 시간을 추출하는 여러 정규 표현식 (다양한 형식 대응)
;;            (date-time-regex-list
;;             '("\\([0-9]\\{8\\}\\)[-_T]?\\([0-9]\\{6\\}\\)?"         ;; YYYYMMDD-HHMMSS
;;               "\\([0-9]\\{8\\}\\)[-_T]?\\([0-9]\\{4\\}\\)"           ;; YYYYMMDD-HHMM
;;               "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)_\\([0-9]\\{2\\}-[0-9]\\{2\\}\\)" ;; YYYY-MM-DD_HH-MM
;;               "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" ;; YYYY-MM-DD
;;               "\\([0-9]\\{13\\}\\)"                                  ;; Unix timestamp
;;               "Pasted image \\([0-9]\\{8\\}\\)\\([0-9]\\{6\\}\\)"    ;; Pasted image YYYYMMDDHHMMSS
;;               ))
;;            new-file-name)
;;       ;; denote 형식인 파일은 건너뜀
;;       (if (denote-id-p base-name)
;;           (message "File %s is already in denote format, skipping." file-name)
;;         ;; 날짜 및 시간 추출 시도
;;         (catch 'done
;;           (dolist (regex date-time-regex-list)
;;             (when (string-match regex base-name)
;;               (let ((date (replace-regexp-in-string "-" "" (match-string 1 base-name)))
;;                     (time (match-string 2 base-name)))
;;                 ;; 시간 형식이 없으면 기본값 "000000" 또는 "0000"일 경우에 "0000" -> "000000"으로 변환
;;                 (setq time (cond
;;                             ((not time) "000000")   ;; 시간 없으면 000000
;;                             ((= (length time) 4) (concat time "00"))  ;; HHMM -> HHMMSS 변환
;;                             (t time)))
;;                 ;; 새로운 파일명 작성 (denote 형식)
;;                 (setq new-file-name (format "%sT%s" date time))
;;                 ;; 동일한 파일명이 있는 경우 고유한 파일명 생성
;;                 (setq new-file-name (generate-unique-file-name directory new-file-name ext))
;;                 ;; 파일명 변경
;;                 (rename-file file-name (expand-file-name new-file-name directory))
;;                 (message "Renamed %s to %s" file-name new-file-name)
;;                 (throw 'done t)))))
;;         (message "No valid date-time found in %s" file-name))))

;;   (defun my/rename-all-screenshot-images-to-denote-id (directory)
;;     "DIRECTORY 내의 모든 JPG, PNG, GIF 파일을 Denote 형식으로 파일명 변경.
;; 이미 denote 형식인 파일은 건너뛰며, 날짜/시간이 없으면 처리하지 않음."
;;     (interactive "DDirectory: ")
;;     (dolist (file (directory-files directory t "^[^.].*")) ;; 숨김 파일 제외
;;       (when (and (file-regular-p file)
;;                  (member (file-name-extension file) '("jpg" "png" "gif")))
;;         (rename-to-denote-id file))))
;;   )

;;;; my/denote-normalize-org-frontmatter

;; https://gist.github.com/ashton314/f74060b00884ac9491b6944dac7bf8de

(defun my/denote-normalize-org-frontmatter ()
  "Normalize the front-matter in an org-mode file."
  (interactive)
  (when-let* ((file (buffer-file-name))
              (denote-filename-is-note-p file)
              (type (denote-filetype-heuristics file))
              (title (denote-retrieve-title-value file type))
              (id (denote-retrieve-filename-identifier file)))
    (let ((kwds (denote-retrieve-keywords-value file type)))
      (delete-matching-lines "^#\\+title:.*$" (point-min) (point-max))
      (delete-matching-lines "^#\\+date:.*$" (point-min) (point-max))
      (delete-matching-lines "^#\\+identifier:.*$" (point-min) (point-max))
      (delete-matching-lines "^#\\+filetags:.*$" (point-min) (point-max))
      (denote--add-front-matter file title kwds id type)
      (delete-matching-lines "^$" (point) (- (point) 1)))))

;;;; provide

(provide 'denote-funcs)

;;;; end-of-line
