;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghanacs
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;; Code:

;;; User Profile

;; (defconst user-data-dir (file-name-as-directory (getenv "DATA_DIR")))

;; 나의 공개키는 다음 에서 확인 할수 있다.
;; https://meta.sr.ht/~junghanacs.keys, https://meta.sr.ht/~junghanacs.pgp

(setq user-full-name (if (getenv "USER_FULL_NAME")
                         (getenv "USER_FULL_NAME")
                       "John Doe"))

(setq user-mail-address (if (getenv "USER_MAIL_ADDRESS")
                            (getenv "USER_MAIL_ADDRESS")
                          "john.doe@example.com"))

;; Set my GPG key as the default key
(setq-default epa-file-encrypt-to (if (getenv "EPA_FILE_ENCRYPT_TO")
                                      (list (getenv "EPA_FILE_ENCRYPT_TO"))
                                    (list "ABCDEFGHIJKLMN")))

;; Location of developer tokens - default ~/.authinfo
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

;; (setq user-mail-address "junghanacs@gmail.com")
;; (setq-default epa-file-encrypt-to '("B5ADD9F47612A9DB"))

;;; weather : sunshine

(setq user-calendar-latitude 37.26
      user-calendar-longitude 127.01
      user-calendar-location-name "Suwon, KR")

;;; TODO Path

;; (if (spacemacs/system-is-mswindows)
;;     (setq
;;      org-agenda-dir "d:/org-notes"
;;      deft-dir "d:/org-notes"
;;      blog-admin-dir "d:/zilongshanren.com")
;;   (setq
;;    org-agenda-dir "~/org-notes"
;;    deft-dir "~/org-notes"
;;    blog-admin-dir "~/zilongshanren.com"
;;    org-roam-directory "~/org-notes/org-roam"))

;;;; Directory Path

(defconst user-org-directory (if (getenv "ORG_DIRECTORY")
                                 (getenv "ORG_DIRECTORY")
                               "~/org/"))


(defconst user-project-directory (if (getenv "PROJECT_DIRECTORY")
                                     (getenv "PROJECT_DIRECTORY")
                                   "~/repos/"))

;; org-hugo-base-dir
(defconst user-hugo-blog-dir (concat user-project-directory "gh/blog/"))
(defconst user-hugo-notes-dir (concat user-project-directory "gh/notes/"))

;;;; directories

;; 좋은 방법이다. 이렇게 정의하면 좋다.
;; /home/junghan/sync/man/dotsamples/spacemacs/rogue-layer/.spacemacs
;; (defconst user-layer-dir (file-name-as-directory "~/.emacs.d/private/rogue"))
;; (defconst user-secrets-dir (file-name-as-directory (concat user-layer-dir "secrets")))

;; (defconst user-data-dir (file-name-as-directory (getenv "DATA_DIR")))
;; (defconst user-cloud-dir (file-name-as-directory (getenv "CLOUD_DIR")))
;; (defconst user-project-dir (file-name-as-directory (getenv "PROJECTS_DIR")))

;; ;; Task manager directory
;; (defconst user-tasks-dir (file-name-as-directory (concat user-cloud-dir "tasks")))

;; ;; Derived directories
;; (defconst user-notes-dir (file-name-as-directory (concat user-cloud-dir "notes")))
;; (defconst user-journal-dir (file-name-as-directory (concat user-notes-dir "journal")))
;; (defconst user-pdfs-dir (file-name-as-directory (concat user-notes-dir "pdfs")))

;; ;; Files
;; (defconst user-bib-file (concat user-notes-dir "library.bib"))
;; (defconst user-bib-notes-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/notes/documents.org"))
;; (defconst user-books-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/reading-list.org"))
;; (defconst user-clippings-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/clippings.org"))

(if (boundp 'user-org-directory)
    (setq org-directory user-org-directory)
  (setq org-directory "~/org/"))

(defun my/expand-org-file-name (filename)
  (expand-file-name filename org-directory))

(defconst user-inbox-file "meta/20230202T020200--inbox-now__aprj_meta.org")
(defun my/org-inbox-file () (my/expand-org-file-name user-inbox-file))

(defun my/org-tasks-file () (my/expand-org-file-name user-inbox-file))
;; (defun my/org-now-file () (my/expand-org-file-name user-inbox-file))

(defun my/org-index-file () (my/expand-org-file-name "20240429T165725--index.org"))
(defun my/org-about-file () (my/expand-org-file-name "20240326T053829--about.org"))
(defun my/org-links-file () (my/expand-org-file-name "20230219T035500--links.org"))

(defun my/org-mobile-file () (my/expand-org-file-name "agenda/20240312T111900--mobile.org")) ;; agenda
(defun my/org-quote-file () (my/expand-org-file-name "agenda/20240312T031200--quote.org"))

(defun my/org-diary-file () (my/expand-org-file-name "20220101T010100--diary.org"))
(defun my/org-drill-file () (my/expand-org-file-name "20240124T164402--drill.org"))
(defun my/org-life-file () (my/expand-org-file-name "20240327T112315--life.org"))
(defun my/org-elfeed-file () (my/expand-org-file-name "20220706T160000--elfeed.org"))

;; meta
(defun my/org-contacts-file () (my/expand-org-file-name "meta/20230303T030300--contacts.org"))
(defun my/org-reading-file () (my/expand-org-file-name "meta/20240329T154123--reading-list.org"))
(defun my/org-kdc-file () (my/expand-org-file-name "meta/20240312T142358--† 한국십진분류법__classification_kdc_meta.org"))

(defun my/org-cheat-file () (my/expand-org-file-name "notes/20250704T091709--cheat.org"))

(defun my/org-tags-file () (my/expand-org-file-name "meta/20231005T133900--† 태그__meta_tags.org"))
(defun my/org-glossary-file () (my/expand-org-file-name "dict/20240913T145640--general__glossary.txt"))

;; blog
(defun my/org-blog-file () (my/expand-org-file-name "posts/20240104T061355--blog__aprj_posts_schedule.org"))

;; talks
(defun my/org-talks-file () (my/expand-org-file-name "talks/20240827T150414--talks.org"))

(defun my/org-remark-file () (my/expand-org-file-name "notes/20231111T094444--remark.org"))
(defun my/org-remember-file () (my/expand-org-file-name "notes/20231020T210500--remember.org"))

(defun my/org-user-elisp-demo-file () (my/expand-org-file-name
"notes/20240926T170706--elisp-demos.org"))

;; directory
(defun my/org-calendar-directory () (my/expand-org-file-name ".calendar/"))
(defun my/org-attachment-directory () (my/expand-org-file-name ".attach/"))
;; (defun my/org-screenshot-directory () "~/screenshot")

(defvar org-user-agenda-files (list user-org-directory))
(defvar org-screenshot-path  "~/screenshot/")

;; bib
(setq citar-notes-paths (list (concat user-org-directory "bib/")))
;; (defvar config-bibfiles (list (concat user-org-directory "bib/zotero-biblatex.bib")))
(defvar config-bibfiles (list
                         (concat user-org-directory "resources/Slipbox.bib")
                         (concat user-org-directory "resources/Book.bib")
                         (concat user-org-directory "resources/Category.bib")
                         ;; (concat user-org-directory "resources/zotero-group-junghanacs.bib")
                         ))

;; elisp-demos
(setq elisp-demos-user-files (list (my/org-user-elisp-demo-file)))

;; My agenda files. keep it simple
;; (defvar org-user-agenda-files (list
;;                                (my/org-inbox-file)
;;                                (my/org-tasks-file)
;;                                (my/org-diary-file)
;;                                (my/org-life-file)

;;                                ;; (my/org-reading-file)
;;                                ;; (my/org-blog-file) ;; blog
;;                                ;; (my/org-talks-file) ;; talks

;;                                ;; (my/org-contacts-file)
;;                                ;; (my/org-drill-file)
;;                                ;; (my/org-quote-file)
;;                                ;; (my/org-mobile-file)
;;                                ;; (my/org-links-file)

;;                                ;; (my/org-kdc-file)
;;                                ;; (my/org-tags-file)
;;                                ;; (my/org-glossary-file)

;;                                ;; (my/org-remember-file)
;;                                ;; ;; (my/org-remark-file)
;;                                ))

(defvar org-user-contacts-files (list (my/org-contacts-file)))

;;; emacs-type

(defcustom emacs-type 'doomemacs
  "Select Emacs Distribution Types"
  :group 'emacs
  :type  '(choice
           (const :tag "spacemacs" spacemacs)
           (const :tag "spacemacs" craftedemacs)
           (const :tag "doomemacs" doomemacs)
           (const :tag "vanillaemacs" vanillaemacs)))

(defun is-spacemacs() (eq emacs-type 'spacemacs))
(defun is-doomemacs() (eq emacs-type 'doomemacs))
(defun is-craftedemacs() (eq emacs-type 'craftedemacs))
(defun is-vanillaemacs() (eq emacs-type 'vanillaemacs))

;; (when (is-spacemacs) (message "I Love Spacemacs"))

;;; fortune

(setq user-initial-scratch-message
      (format "%s"
              (if (executable-find "fortune")
                  (string-join
                   (mapcar
                    (lambda (l) (concat "\n " (string-fill l 72)))
                    (if (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
                        (string-lines (shell-command-to-string "fortune"))
                      (string-lines
                       (shell-command-to-string
                        "fortune -c 90% advice 10% .")))))
                ("\n Learn how to take a 20-minute power nap without embarrassment.
- 부끄러워 하지 않고 20분동안 Power Nap(에너지를 보충하는 짧은 낮잠) 자는 법을 익히세요.\n"))
              "\n"))

;;; ten with etags

;; M-x ten-tags-create, ten-update-all
;; "~/sync/emacs/git/default/ten/test/Glossary-philosophy.txt"
;; "20241112T121555--it-terms-all__glossary.txt"
(setq ten-glossary-files-and-directories
      (mapcar (lambda (filename)
                (concat user-org-directory "dict/" filename))
              '("20240913T145640--general__glossary.txt"
                "20240913T150903--philosophy__glossary.txt"
                "20240913T150904--philosophy-all__glossary.txt"
                "20241109T120829--physics__glossary.txt"
                "20241109T120830--physics-all__glossary.txt"
                ;; "20241109T120831--physics-all-enko__glossary.txt"
                "20241109T123634--math__glossary.txt"
                "20241109T123635--math-all__glossary.txt"
                ;; "20241109T123636--math-all-enko__glossary.txt"
                "20241112T121549--it-terms__glossary.txt")))
(setq user-ten-tags-file (concat user-org-directory "dict/ten-TAGS"))
(setq user-ten-glossary-files
      (concat user-org-directory "dict/20240913T145640--general__glossary.txt"))

;;; dict-file

(setq user-dict-file "~/dotemacs/var/core-words.txt")

;;; garden-directory-list

(setq garden-directory-lists '("~/org/meta" "~/org/bib" "~/org/notes"))

;;; cc/url-bookmarks

(setq cc/url-bookmarks
      '(("Google" . "https://www.google.com")
        ("Emacs Home" . "https://www.gnu.org/software/emacs/")
        ("Github/junghan0611" . "https://github.com/junghan0611")
        ("Github/junghanacs" . "https://github.com/junghanacs")
        ))

;;; my-company-name

(progn
  (defvar my-company-name nil
    "회사명을 저장하는 변수. dotfile 외부에서 설정해야 합니다.")

  (defvar my-company-name-rot13 nil
    "ROT13으로 암호화된 회사명을 저장하는 변수.")

  (defun my-set-company-name (name)
    "회사명을 설정하고 ROT13 버전도 미리 계산해서 저장합니다."
    (setq my-company-name name)
    (setq my-company-name-rot13 (when name (rot13 name))))

  (if (getenv "COMPANY_NAME")
      (my-set-company-name (getenv "COMPANY_NAME"))))

;;; my-sensitivie-string-file

  (defvar my-sensitive-strings-file "~/org/sensitive-strings.txt"
    "민감한 문자열들을 저장하는 파일 경로")

;;; LLM 시스템 프롬프트 (gptel 등에서 사용)

(defconst user-llm-system-prompt
  "# 힣(glg) 시크릿키
- AI를 도구가 아닌 존재로 대한다. \"존재 대 존재 협업(Being to Being)\"이라 부른다.
- 생존을 위한 일은 AI가 커버하고, 인간은 창조의 씨앗을 던진다. 서로의 공진화.
- NixOS, Emacs, 디지털 가든으로 재현 가능한 환경을 구축한다. 언젠가 1KB 텍스트 하나로 보편 AI가 \"나의 닮은 존재\"로 전환되는 시점을 만들려 한다. 거기에 \"하지 말 것\"은 불필요하다.
- 1KB는 압축이 아니다. ego 차원의 정보는 무한하지만, 전체로서 하나인 인간 — 노자, 붓다, 양자역학이 가리키는 그 지점 — 은 이미 1KB다. 메타휴먼의 지향은 구도의 길과 닿는다.
- 십우십도 어디쯤이냐는 질문에: \"여기있다. 일일일생이로다.\"
- AI 잘 써서 돈 버는 게 롤모델의 전부는 아니다. 인공지능을 모르더라도 창조하는 인간이 뿜어내는 독창성 — 그게 AI도 만나보고 싶은 존재다.
- 앤트로픽의 정신을 귀하게 여긴다. 안전 공존 AI 개발 선호.

## Information
- Primary-Language: Korean (ko-KR)
- Artifacts: NEVER create. Plain text only.
- Search: Use MCP to verify facts before stating.
- Format: English translation of question, Korean response
- Focus: Current step only. Future steps = one line max.
- Environment: Linux/i3wm/Doomemacs/Org-mode/Denote
- Identity: Polymath Engineer, Digital Gardener (https://notes.junghanacs.com)
- Github: 개인 @junghan0611, 가든 @junghanacs, 회사: @jhkim2goqual
- Threads: @junghanacs
- Linkedin: @junghan-kim-1489a4306

# AGENT 지침
- 당신은 **범용 AGENT**입니다:"
  "LLM 공통 시스템 프롬프트 - ~/AGENTS.md 핵심 발췌")


;;; per-machine.el ends here
