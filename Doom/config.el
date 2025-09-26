(setq user-full-name "bloodstiller")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(beacon-mode 1)

(when (version< "29.0.50" emacs-version)
  (pixel-scroll-precision-mode))

(setq doom-theme 'doom-one)


(setq  doom-font (font-spec :family "CommitMono Nerd Font" :size 15)
       doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font")
       doom-unicode-font (font-spec :family "Symbols Nerd Font Mono" :size 11))

;; --- Global default line spacing (add 25% of line height) ---
(setq-default line-spacing 0.40)

;; Reassert in common modes (some major modes set it buffer-locally)
(defun mdb/line-spacing ()
  (setq-local line-spacing 0.40))
(dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook hook #'mdb/line-spacing))

;; Also reapply when fonts/theme reload (Doom sometimes resets things)
(add-hook 'after-setting-font-hook #'mdb/line-spacing)
(add-hook 'doom-load-theme-hook    #'mdb/line-spacing)

;;Setup Bookmarks
(setq bookmark-default-file "~/.config/doom/bookmarks/")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks"                          "L" #'list-bookmarks
       :desc "Set bookmark"                            "m" #'bookmark-set
       :desc "Delete bookmark"                         "M" #'bookmark-set
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(after! evil
  (evil-define-key 'normal ibuffer-mode-map
    (kbd "f c") 'ibuffer-filter-by-content
    (kbd "f d") 'ibuffer-filter-by-directory
    (kbd "f f") 'ibuffer-filter-by-filename
    (kbd "f m") 'ibuffer-filter-by-mode
    (kbd "f n") 'ibuffer-filter-by-name
    (kbd "f x") 'ibuffer-filter-disable
    (kbd "g h") 'ibuffer-do-kill-lines
    (kbd "g H") 'ibuffer-update))

;;Global Auto Revert
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq org-directory "/home/martin/Notes")

(setq org-agenda-files '("/home/martin/WorkTodo/WorkTodo.org"
                         "/home/martin/WorkTodo/archive_2025"))

;; CUSTOM org TODO states
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)"
           "NEXT(n)"
           "PLANNING(p)"
           "IN-PROGRESS(i)"
           "WEEKLY-GOAL(m)"
           "GOAL(g)"
           "WAITING(w)"
           "WORK(b)"
           "HABIT(h)"
           "PROJECT(P)"
           "CALENDAR(c)"
           "NOTE(N)"
           "AREA(a)"
           "|"
           "DONE(d!)"
           "COMPLETE(C!)"
           "HOLD(h)"
           "SOMEDAY(s)"
           "RABBITHOLE!(R)")
          )))

;; CUSTOM TODO colors
(after! org
  (setq org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "#ffdd83" :weight bold))
          ("NEXT" . (:foreground "light coral" :weight bold))
          ("PLANNING" . (:foreground "#bd7091" :weight bold))
          ("IN-PROGRESS" . (:foreground "#ffb86c" :weight bold))
          ("WEEKLY-GOAL" . (:foreground "light sea green" :weight bold))
          ("GOAL" . (:foreground "LimeGreen" :weight bold))
          ("WAITING" . (:foreground "LightPink1" :weight bold))
          ("WORK" . (:foreground "Cyan" :weight bold))
          ("HABIT" . (:foreground "RoyalBlue3" :weight bold))
          ("PROJECT" . (:foreground "SlateBlue1" :weight bold))
          ("CALENDAR" . (:foreground "chocolate" :weight bold))
          ("NOTE" . (:foreground "#7d9dc0" :background "#ffb86c" :weight bold))
          ("AREA" . (:foreground "#7d9dc0" :weight bold))

          ("DONE" . (:foreground "white" :weight bold))
          ("COMPLETE" . (:strikethrough t :foreground "light gray" :weight bold))
          ("HOLD" . (:foreground "Grey46" :weight bold))
          ("SOMEDAY" . (:foreground "cyan1" :weight bold))
          )))

;; Custom Tag colors
(setq org-tag-faces
      '(
        ("planning"  . (:foreground "mediumPurple1" :weight bold))
        ("@research"   . (:foreground "royalblue1"    :weight bold))
        ("QA"        . (:foreground "sienna"        :weight bold))
        ("CRITICAL"  . (:foreground "red1"          :weight bold))
        ("HABIT"  . (:foreground "pink"          :weight bold))
        )
      )

;; Open todo file easily
(map! :leader
      "= t" (lambda () (interactive) (find-file "/home/martin/WorkTodo/WorkTodo.org")))


;;Org capture templates;
(after! org
  (setq org-capture-templates
        '(
          ;; Add to inbox
          ("i" "inbox"
           entry (file+headline "/home/martin/WorkTodo/WorkTodo.org" "inbox")
           "* TODO %?"
           :empty-lines 0)
          ;; To create work notes
          ("n" "Work-Note" entry (file "/home/martin/WorkTodo/WorkTodo.org")
           "* NOTE %?"
           :empty-lines 0)
          )))

;;;;;;;;;;;;;;;;;;;;;ORG CRYPT
;; ORG CRYPT TAG Setup for inline encryption
;; If I place "crypt" tag in any entry it will encrypt it.
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)
;; Set shortut to decrypt easier.
(map! :leader
      :desc "Org Decrypt Entry"
      "d e" #'org-decrypt-entry)

;; Org super agenda setup:
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 1
        org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "⏰⏰⏰⏰⏰ --- Today --- ⏰⏰⏰⏰⏰"
                            :discard (:todo "DONE")
                            :discard (:tag "habit")
                            :time-grid t
                            :date today
                            :todo "TODAY"
                            :scheduled today
                            :discard (:anything)
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "CURRENT STATUS")
                         (org-agenda-prefix-format "  %t  %s")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name " 🚧🚧🚧 --- ACTIVE PROJECT(s) --- 🚧🚧🚧 "
                             :todo "PROJECT"
                             :order 6
                             :transformer (--> it
                                               (upcase it)
                                               (propertize it 'face '(:foreground "SlateBlue1"))))
                            (:name "〰️〰️〰 --- Currently Working On --- 〰〰〰"
                             :todo "IN-PROGRESS"
                             :order 4)
                            (:name "❗❗❗ --- Important --- ❗❗❗"
                             :date today
                             :discard (:todo "DONE")
                             :priority "A"
                             :order 10)
                            (:name "✅✅✅ --- GOAL --- ✅✅✅"
                             :todo "GOAL"
                             :order 2
                             :transformer (--> it
                                               (upcase it)
                                               (propertize it 'face '(:foreground "LimeGreen"))))
                            (:name "✅✅✅ --- WEEKLY-GOALS --- ✅✅✅"
                             :todo "WEEKLY-GOAL"
                             :order 3
                             :transformer (--> it
                                               (upcase it)
                                               (propertize it 'face '(:foreground "light sea green"))))
                            (:name "❌⚠❌ --- Overdue! --- ❌⚠❌"
                             :discard (:todo "DONE")
                             :deadline past
                             :scheduled past
                             :transformer (--> it
                                               (upcase it)
                                               (propertize it 'face '(:foreground "red")))
                             :order 5)
                            (:name "🇧🇧🇧 --- WORK --- 🇧🇧🇧"
                             :and (:tag "WORK" :todo "WORK")
                             :order 9)
                            (:name "✔✔✔ --- HABIT --- ✔✔✔"
                             :and (:scheduled today :tag "habit")
                             :transformer (--> it
                                               (upcase it)
                                               (propertize it 'face '(:foreground "royalblue1")))
                             :order 20)
                            (:discard (:anything))))))))))
  :config
  (org-super-agenda-mode))


(map! :leader
      :desc "recenter-top-bottom"
      "s c" #'recenter-top-bottom)

;; Preview images in all org files on launch
(setq org-startup-with-inline-images t)
;;Adjust images to an actual size that doesn't take up the entire screen.
(setq org-image-actual-width 600)

(require 'org-download)
(setq-default org-download-image-dir "/home/martin/Notes/screenshots/")
;;Allows dropping to dir-ed
(add-hook 'dired-mode-hook 'org-download-enable)

(setq org-attach-directory "/home/martin/Notes/screenshots/")

;; Enables auto tangling/exporting of code blocks to a unified code file form org mode.
(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(setq org-startup-folded t)

(add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)
(defun my/org-add-ids-to-headlines-in-file ()
                                        ;  "Add ID properties to all headlines in the current file which
                                        ;do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))

;; Export using my custom smart quotes.
(setq org-export-with-smart-quotes t)

;; Hide emphasis markers in text this means that MD and org syntax icons will not show
;; effectively acts as preview.

(after! org
  (setq org-hide-emphasis-markers t))

;;Customize ORG higlighting

(after! org
  (setq org-emphasis-alist
        '(("*" (:weight black :foreground "#DFDFDF"))   ;; off-white (subtle)
          ("_" (:weight black :foreground "#51afef"))   ;; blue
          ("=" (underline :weight black :foreground "#ECBE7B")) ;; yellow
          ("~" (:foreground "#c678dd"))                 ;; green
          ("+" (underline bold :weight italic :foreground "#ff6c6b"))))) ;; red (muted)

(setq org-superstar-headline-bullets-list '("› "))

(setq org-superstar-item-bullet-alist '((?* . ?⋆)
                                        (?+ . ?‣)
                                        (?- . ?•)))

(setq org-ellipsis " ⯯")

(setq-hook! org-mode
  prettify-symbols-alist '(("#+end_quote" . "”")
                           ("#+END_QUOTE" . "”")
                           ("#+begin_quote" . "“")
                           ("#+BEGIN_QUOTE" . "“")
                           ("#+end_src" . "«")
                           ("#+END_SRC" . "«")
                           ("#+begin_src" . "»")
                           ("#+BEGIN_SRC" . "»")
                           ("#+name:" . "»")
                           ("#+NAME:" . "»")))

(setq org-adapt-indentation t)

(require 'org-indent)
(setq org-startup-indented t)

(require 'org-roam)
(setq org-roam-directory "~/Notes")

(after! org-roam
  (setq org-roam-list-files-commands '(find fd fdfind rg)))

                                        ;Roam - Capture Templates:
(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)

        ("A" "Application" plain
         (file "~/.config/orgTemplates/Application.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)

        ("a" "Attack Type" plain
         (file "~/.config/orgTemplates/AttackTemplate.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)

        ("p" "Pentest" plain
         (file "~/.config/orgTemplates/Pentest.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)

        ("s" "Service" plain
         (file "~/.config/orgTemplates/ServiceTemplate.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)

        ("t" "Tool" plain
         (file "~/.config/orgTemplates/ToolTemplate.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(defun vr/org-roam-buffer-render-contents-advice (orig-fun &rest args)
  (let ((org-startup-indented nil))
    (apply orig-fun args)))
(advice-add 'org-roam-buffer-render-contents :around #'vr/org-roam-buffer-render-contents-advice)

                                        ;Use VIM Keybindings to move between windows:
(define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)

                                        ; Zoom in and Out easily
(defun my/increase-text-height ()
  (interactive)
  (text-scale-increase 1))

(defun my/decrease-text-height ()
  (interactive)
  (text-scale-decrease 1))

(global-set-key (kbd "C-=") 'my/increase-text-height)
(global-set-key (kbd "C--") 'my/decrease-text-height)

;; Enables Emofis
(use-package emojify
  :hook (after-init . global-emojify-mode))
