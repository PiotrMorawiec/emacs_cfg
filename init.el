;; -*- lexical-binding: t; -*-
(message "Start reading ~/.emacs.d/init.el ...")

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)

;; ==============================================================
;; CUSTOM FUNCTIONS
;; ==============================================================

(defun my/revert-buffer ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun my/kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun my/kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (my/kill-thing-at-point 'word))

(defun my/kill-sentence-at-point ()
  "Kill the sentence at point."
  (interactive)
  (my/kill-thing-at-point 'sentence))

(defun my/scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun my/scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(defun my/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
            If there's no region, the current line will be duplicated. However, if
            there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end) (point))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun my/toggle-highlight-trailing-whitespaces ()
  "Function toggles highlighting trailing whitespaces"
  (interactive)
  (if (bound-and-true-p show-trailing-whitespace)
      (progn  (message "Disable highlighting of trailing whitespaces")
              (setq-default show-trailing-whitespace nil))
    (progn (message "Enable highlighting of trailing whitespaces")
           (setq-default show-trailing-whitespace t))))



(defun my/toggle-idle-highlight-mode ()
  "Function toggles 'idle-highlight-mode'"
  (interactive)
  (if (bound-and-true-p dle-highlight-mode)
      (progn  (message "Disable 'idle-highlight-mode'")
              (setq-default idle-highlight-mode nil))
    (progn (message "Enable 'idle-highlight-mode'")
           (setq-default idle-highlight-mode t))))

(defun my/which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

(defun my/untabify-entire-buffer ()
  (interactive)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (message "Converting all TAB's to spaces")
  (keyboard-quit))

(defun my/open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  (message "Init file opened"))

;; Function copied from Emacs Wiki (https://www.emacswiki.org/emacs/KillingBuffers)
(defun my/close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))


;; Function copied from Emacs Wiki (https://www.emacswiki.org/emacs/KillingBuffers)
(defun my/close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun my/other-window-kill-buffer ()
  "Function woks when there are multiple windows opened in the current frame.
         Kills the currently opened buffer in all the other windows"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

(defun my/kill-other-buffers ()
  "Kill all other buffers except the active buffer."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer) (buffer-list))))

;; TODO: prevent function from removing *Messages buffer
;; https://stackoverflow.com/questions/1687620/regex-match-everything-but-specific-pattern
(defun my/kill-asterisk-buffers ()
  "Kill all buffers whose names start with an asterisk (‘*’).
         By convention, those buffers are not associated with files."
  (interactive)
  (kill-matching-buffers "*" nil t)
  (message "All asterisk (*) buffers have been killed"))

(defun my/reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name))
        (warn "Package %s failed to reinstall" package-name)))))

(defun my/reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

;; ==============================================================
;; BASIC UI CONFIG
;; ==============================================================

;; Set startup screen photo
;; (setq fancy-splash-image "path")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(blink-cursor-mode 1)
(delete-selection-mode 1)
(column-number-mode 1)

;; Enable / disable displaying LR/CR characters
;; (global-whitespace-mode nil)

;; Enable mouse support in terminal Emacs
(xterm-mouse-mode 1)

;; Enable auto revert mode globally, so that all buffers will be in sync with whats actually on disk.
;; If you are sure that the file will only change by growing at the end, use Auto Revert Tail mode instead, as
;; it is more efficient for this.
(global-auto-revert-mode t)


;; How to get colors in terminal Emacs ?
;; https://www.gnu.org/software/emacs/manual/html_mono/efaq.html#Colors-on-a-TTY

;; Mouse behaviour
(setq mouse-wheel-progressive-speed nil)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(92 . 92))
(add-to-list 'default-frame-alist '(alpha . (92 . 92)))

;; Enable full screen
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Do not create backup files (with ~ suffix)
(setq make-backup-files nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-hl-line-mode 1)
(set-face-background hl-line-face "gray13")

(setq-default show-trailing-whitespace nil)
(setq-default explicit-shell-file-name "/bin/bash")

;; ==============================================================
;; ADDITIONAL CONFIG FILES
;; ==============================================================

;; Do not use `init.el` for `custom-*` code (generated by 'M-x customize' menu) - use `custom-file.el`.
(setq custom-file "~/.emacs.d/custom-file.el")

;; Use default Emacs bookmarks localisation (for now)
(setq bookmark-default-file "~/.emacs.d/bookmarks")

;; Assuming that the code in custom-file is execute before the code
;; ahead of this line is not a safe assumption. So load this file
;; proactively.
(load-file custom-file)

(setq url-proxy-services nil)

;; ==============================================================
;; PACKAGE REPOSITORIES
;; ==============================================================

(require 'package)

(add-to-list 'package-archives '("gnu"          . "https://elpa.gnu.org/packages/")     t)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/")        t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Load Emacs Lisp packages, and activate them - variable ‘package-load-list’ controls which packages to load.
(package-initialize)

;; Update list of available packages - sth like 'git fetch'
;; doing it together with 'unless' reduces emacs startup time significantly
(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
        (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package spacemacs-theme
  :ensure t
  :defer t
  :custom
        (setq spacemacs-theme-comment-bg nil)
        (setq spacemacs-theme-comment-italic t)
  :init (load-theme 'spacemacs-dark t))

;; You will most likely need to adjust this font size for your system!
(defvar my/default-font-size 130)
(defvar my/default-variable-font-size 130)

(set-face-attribute 'default nil :font "Fira Code Retina" :height my/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height my/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/default-variable-font-size :weight 'regular)

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Let's code ;)")
  (setq dashboard-startup-banner "~/.emacs.d/img/pm_profile_scaled.png")
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((projects  . 5)
                          (agenda    . 3)))
  (dashboard-setup-startup-hook))

;; ==============================================================
;; PACKAGES
;; ==============================================================

(use-package all-the-icons
  :ensure t)

;; This should be invoked on a given machine only once
;; (all-the-icons-install-fonts)

;; Test all-the-icons package with executing (C-x C-e)
;; (all-the-icons-insert-alltheicon)

(defun my/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  )

(defun my/org-mode-setup ()
  (interactive)
  (org-indent-mode)
  ;; Turn on variable-pitch mode in org buffers.
  ;; That will make all the fonts which were not explicitly set to fixed-pitch, to be variable-pitch
  (variable-pitch-mode 1)
  ;; Enable text wrapping in org-mode (it looks better when side piddings enbaled)
  (visual-line-mode 1))

(use-package org
  :ensure t
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  ;; start org-agenda in log-mode by default (like if 'a' option was chosen)
  (setq org-agenda-start-with-log-mode t)
  ;; whenever task is DONE - add information (log) about when the task has been finished
  (setq org-log-done 'time)
  ;; Hide org emphasis characters, like *, =, -, + etc.
  (setq org-hide-emphasis-markers t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (my/org-font-setup))

;; Bind certain org emphasis functionalities to certain keys
(setq org-emphasis-alist
      (quote (("*" bold)
	      ("/" italic)
	      ("_" underline)
	      ("=" (:foreground "orange" :background inherit))
	      ("~" org-verbatim verbatim)
	      ("+"
	       (:strike-through t))
	      )))

(defun my/org-mode-visual-fill ()
  "Function imposes left and right side paddings in org-mode"
  (interactive)
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Package that allows left/right side padding in org mode
(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(use-package org-bullets
  :ensure t
  :after org
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(add-hook 'org-mode-hook #'org-bullets-mode)

;; This package enables org notifications on your OS desktop
(use-package org-wild-notifier
  :ensure t)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org_roam_database")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "meeting" plain
      (file "~/org_roam_database/templates/meeting_template.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
      :unnarrowed t)
     ("e" "words" plain
      (file "~/org_roam_database/templates/words_template.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
      :unnarrowed t)
     ("t" "todo list" plain
      (file "~/org_roam_database/templates/todos_template.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
      :unnarrowed t)
     ("p" "private agenda" plain
      (file "~/org_roam_database/templates/private_agenda_template.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
      :unnarrowed t)
     ("w" "work agenda" plain
      (file "~/org_roam_database/templates/work_agenda_template.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
      :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
         ("I" . my/org-roam-node-insert-immediate))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap "org-roam-dailies-map" is available
  (org-roam-db-autosync-mode))

(defun my/org-roam-node-insert-immediate (arg &rest args)
  "Function allows to onsert/link a new note without the necessity of filling this note at the moment,
   so you can go back later and fill those notes in with more details"
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  "Function filters Org Roam files by given tag.
   Tags are specified in Org Roam files in '#+filetags:' section."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  "Function returns list composed of all Org Roam files, containing given tag"
  (interactive)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (require 'org-roam)
  (setq org-agenda-files (append (my/org-roam-list-notes-by-tag "todos")
                                 (my/org-roam-list-notes-by-tag "work_agenda")
                                 (my/org-roam-list-notes-by-tag "private_agenda"))))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

;; Do not ask for permission to execute code block
(setq org-confirm-babel-evalauate nil)

;; Set (overwrite) default ORG Babel Header Arguments, for all code blocks.
;; See: https://orgmode.org/manual/Using-Header-Arguments.html
(setq org-babel-default-header-args
      (cons '(:tangle . "yes")
            (assq-delete-all :noweb org-babel-default-header-args)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (octave . t)
   (emacs-lisp . t)))

;; Set Babel to use Python 3
(setq org-babel-python-command "python3")

;; Enable unix-like configuration language (used in plenty of Unix configuration files)
(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

;; Type for example
;;   - <py followed by TAB to insert python clode block
;;   - <el followed by TAB to insert elisp  clode block
(add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
(add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py"  . "src python"))
(add-to-list 'org-structure-template-alist '("sv"  . "src verilog"))
(add-to-list 'org-structure-template-alist '("vhd" . "src vhdl"))

;; Automatically tangle our Emacs.org config file when we save it
(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

(defun my/lsp-mode-setup ()
  "Function configures LSP by disabling/enabling particular LSP features
     See:
        https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/"
  ;; Configure headerline
  (setq lsp-headerline-breadcrumb-segments '(file symbols))
  (lsp-headerline-breadcrumb-mode)
  ;; Disable linter by default, as it is annoying (Flycheck / Flymake)
  (setq lsp-diagnostics-provider :none)
  ;; Configure LSP modeline features
  (setq lsp-modeline-code-actions-mode-segments '(count icon name))
  (lsp-modeline-code-actions-mode))


;; Language Server Protocol support
(use-package lsp-mode
  :ensure t
  :after (which-key)
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . my/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :custom
  (lsp-clients-svlangserver-launchConfiguration "verilator -sv --lint-only -Wall")
  (lsp-clients-svlangserver-formatCommand "verible-verilog-format")
  :config
  (lsp-enable-which-key-integration t))

;; This package is reposnsible for displaying auxiliary informations on symbols
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))


;; Great source of information about flyckeck:
;; https://www.masteringemacs.org/article/spotlight-flycheck-a-flymake-replacement
(use-package flycheck
  :ensure t
  :custom
  (global-flycheck-mode nil))

(use-package lsp-treemacs
  :after (lsp treemacs))

(use-package helm-lsp
  :after (lsp helm))

;; (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  ;; amount of letters need to be already typed in order to start completion
  (company-minimum-prefix-length 1)
  ;; time delya before starting completion
  (company-idle-delay 0.0))


(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(defun my/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  ;; (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        ;; If a command was executen multiple times in a row, save in in history only once
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

;; This package loads fancy eshell prompts for GIT users
;; To set given prompt, type M-x eshell-git-prompt-use-theme
(use-package eshell-git-prompt
  :ensure t)

(use-package eshell
  :hook (eshell-first-time-mode . my/configure-eshell)
  :config

  ;; Eshell is comprised of a series of packages, and sometimes you have to hook
  ;; your configurtion after the load of a particular package so that it works correctly.
  ;; esh-opt is one of those packages.
  ;; This is one of those  oddities of configuration for eshell.
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    ;; The commands blow will be in fact run in term-mode
    ;; as those doesn't always works correctly on Eshell
    (setq eshell-visual-commands '("ssh" "htop" "zsh" "vim")))

    (eshell-git-prompt-use-theme 'powerline))

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")
;; Set default user
(setq tramp-default-user "pi")
;; Set default host
(setq tramp-default-host "192.168.1.5")

(use-package dired
  :ensure nil ;; dires is a built-in emacs package, so don't look for it in package repositories
  :commands (dired dired-jump) ;; defer this config until one of this commands is executed
  :bind (("C-x j" . dired-jump)
         ;; those bindings will only be valid if dired-mode is active
         :map dired-mode-map
         ;; change this from ^ which is not convenient
         ("<C-backspace>" . dired-up-directory)
         ;; this one is a default keybinding, keep it here as an information tough
         ("v" . dired-view-file))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  )

;; Thanks to this package, the directories that we've visited won't be existing as opened buffers.
;; Instead, all these buffers will be closed automatically.
(use-package dired-single
  :after (dired)
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ("<C-return>" . dired-single-up-directory)
              ("<return>"   . dired-single-buffer)))

;; This package has been replaced with "treemacs-icons-dired"
;; (use-package all-the-icons-dired)

;;  This package allow us to set a program different than Emacs, that we want to open given files with
(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "gwenview")
                                ("jpg" . "gwenview"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map ("h" . dired-hide-dotfiles-mode)))

(use-package go-translate
  :ensure t
  :custom
  ;; Confiugre language pairs used to transale
  (gts-translate-list '(("en" "pl") ("pl" "en")))
  ;; Configure the default transanslator (used by gts-do-transalte)
  (gts-default-translator
   (gts-translator
    :picker (gts-prompt-picker)
    :engines (list (gts-bing-engine) (gts-google-engine))
    :render (gts-buffer-render))))

(defun my/translate-region ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-noprompt-picker)
                  :engines (list (gts-bing-engine) (gts-google-engine))
                  :render (gts-buffer-render))))

(defun my/translate-region-pop-render ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-prompt-picker)
                  :engines (list (gts-bing-engine) (gts-google-engine))
                  :render (gts-posframe-pop-render))))

(defun my/translate-region-pin-render ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-prompt-picker)
                  :engines (list (gts-bing-engine) (gts-google-engine))
                  :render (gts-posframe-pin-render))))

(use-package command-log-mode
  :ensure t
  :bind (("C-c c t" . clm/toggle-command-log-buffer)
         ("C-c c o" . clm/open-command-log-buffer)
         ("C-c c x" . clm/close-command-log-buffer)
         ("C-c c c" . clm/command-log-clear)
         ("C-c c s" . clm/save-command-log)
         )
  :custom
  ;; disable default keybinding "C-c o" that opens command-log-buffer
  (command-log-mode-key-binding-open-log nil)
  :config
  ;; Enable command-log-mode globally by default
  (global-command-log-mode t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package idle-highlight-mode
  :ensure t
  :custom
  (idle-highlight-idle-time 0.1)
  :hook
  ((prog-mode text-mode) . idle-highlight-mode))

(use-package highlight-indent-guides
  :ensure t
  :custom
  (highlight-indent-guides-method (quote character)))

(use-package drag-stuff
  :ensure t)

(use-package dumb-jump
  :ensure t)

;; Load Silver Searcher
(use-package ag
  :ensure t)

;; Load ripgrep
(use-package rg
  :ensure t)

(use-package xref
  :ensure t)

(use-package doom-modeline
  :after (all-the-icons)
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-project-detection 'auto
        doom-modeline-height 40)
  :custom
  (display-battery-mode t))

;; (use-package keycast
;;   :config
;;   ;; This works with doom-modeline, inspired by this comment:
;;   ;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
;;   (define-minor-mode keycast-mode
;;     "Show current command and its key binding in the mode line."
;;     :global t
;;     (if keycast-mode
;; 	(add-hook 'pre-command-hook 'keycast--update t)
;;       (remove-hook 'pre-command-hook 'keycast--update)))
;;   (add-to-list 'global-mode-string '("" mode-line-keycast " "))
;;   (keycast-mode nil))

(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode t))

(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t)
  :diminish nil)

(use-package magit
  :ensure t
  :custom
  (magit-status-buffer-switch-function 'switch-to-buffer)
  :bind (("C-c g s" . magit-status)
         ("C-c g f" . magit-fetch)
         ("C-c g b" . magit-blame)
         ("C-c g r" . magit-branch)
         ("C-c g c" . magit-checkout)))

(use-package projectile
  :ensure t
  :bind (
         ("<f7>"  . projectile-add-known-project)
         )
  :init
  (projectile-mode 1))

(use-package treemacs
  :ensure t
  :bind (
         ("<C-f5>" . treemacs)
         ("<C-f6>" . treemacs-add-project-to-workspace)
         )
  :config
  (treemacs-follow-mode t)
  (treemacs-git-mode 'deferred)
  ;; Make tremacs display nice indents in files hierarchy
  ;; (treemacs-indent-guide-mode 't)
  ;; (treemacs-indent-guide-style 'line)
  :custom
  ;; This fixes problem with helm buffers (e.g. helm-M-x)
  ;; ocupying the entire widow
  (treemacs-display-in-side-window nil)
  ;; Set default treemacs width, and unlock the
  ;; drag-and-drop resize option
  (treemacs-width 50)
  (treemacs-width-is-initially-locked nil)
  ;; Disable test wrapping in treemacs window, when widnow is to narrow
  (treemacs-wrap-around nil)
  )

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs all-the-icons))

(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package helm
  :ensure t
  :bind  (("M-x"     . helm-M-x)
          ("M-y"     . helm-show-kill-ring)
          ("C-x C-f" . helm-find-files)
          ("C-b"     . helm-buffers-list)
          ("C-c h o" . helm-occur)
          ("C-c h b" . helm-filtered-bookmarks)
          )
  :custom
  (helm-position 'bottom)
  ;; This fixes problem with helm buffers (e.g. helm-M-x)
  ;; ocupying the entire widow.
  ;; Although "helm-split-window-in-side-p" is deprecated
  ;; and superseeded "helm-split-window-inside-p", both
  ;; variables have to be set to t.
  (helm-split-window-in-side-p t)
  (helm-split-window-inside-p t)
  :init
  (helm-mode 1)
  (helm-autoresize-mode 1))

;; Ned to apply these changes in order to make "helm-icons" work together with dired buffers:
;; - thread: https://github.com/yyoncho/helm-icons/issues/16
;; - code changes: https://github.com/yyoncho/helm-icons/pull/17/commits/eead11e9bdb2b8f3e1c7464953cc5ca70388f564
(use-package helm-icons
  :ensure t
  :after (all-the-icons helm)
  :custom
  (helm-icons-provider 'all-the-icons)
  :config
  (helm-icons-enable))

(use-package helm-swoop
  :ensure t
  :bind
  (("M-s". helm-swoop))
  :custom
  ;; This decreases helm swoop speed but in favour of colorded results
  (helm-swoop-speed-or-color t)
  ;; This fixes problem with helm-swoop appearing in another window,
  ;; when using multiple windows in one frame (treemacs / minimap)
  (helm-swoop-split-with-multiple-windows t)
  )

(use-package helm-xref
  :ensure t
  :after helm
  :commands helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :bind(
        ("C-p"   . helm-projectile-find-file)
        ("C-l"   . helm-projectile-recentf)
        ("<f8>"  . helm-projectile-switch-project)
        ))

(use-package helm-ag
  :ensure t)

(use-package minimap
  :ensure t
  :custom
  (minimap-always-recenter nil)
  (minimap-hide-fringes t)
  (minimap-hide-scroll-bar nil)
  (minimap-highlight-line nil)
  (minimap-minimum-width 20)
  (minimap-recenter-type (quote relative))
  (minimap-recreate-window t)
  (minimap-update-delay 0)
  (minimap-width-fraction 0.06)
  (minimap-window-location (quote right))
  :custom-face
  (minimap-active-region-background ((((background dark)) (:background "#3c3c3c" :extend t)) (t (:background "#C847D8FEFFFF" :extend t))))
  (minimap-font-face ((t (:weight bold :height 15 :width normal :family "DejaVu Sans Mono"))))
  :config
  (minimap-mode -1))

(use-package verilog-mode
  :ensure t
  :custom
  (verilog-align-ifelse t)
  (verilog-auto-delete-trailing-whitespace t)
  (verilog-auto-indent-on-newline t)
  (verilog-auto-newline nil)
  (verilog-highlight-grouping-keywords t)
  (verilog-highlight-modules t)
  (verilog-indent-level 2)
  (verilog-indent-level-behavioral 2)
  (verilog-indent-level-declaration 2)
  (verilog-indent-level-directive 0)
  (verilog-indent-level-module 2))

(use-package python-mode
  :ensure t
  :hook
  (python-mode . lsp-deferred)
  (python-mode . (lambda ()
                   (setq indent-tabs-mode nil)
                   (setq tab-width 4)
                   (setq python-indent-offset 4)))
  :custom
  (python-shell-interpreter "python3"))

(use-package yaml-mode
    :ensure t)

  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  ;; Unlike python-mode, this mode follows the Emacs convention of not
  ;; binding the ENTER key to `newline-and-indent'.  To get this
  ;; behavior, add the key definition to `yaml-mode-hook':
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; (use-package paredit
;;   :ensure t
;;   :init
;;   (add-hook 'clojure-mode-hook #'enable-paredit-mode)
;;   (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook #'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook #'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;   (add-hook 'scheme-mode-hook #'enable-paredit-mode)
;;   :config
;;   (show-paren-mode t)
;;   (paredit-mode t)
;;   :bind (("M-[" . paredit-wrap-square)
;;       ("M-{" . paredit-wrap-curly))
;;   :diminish nil)

;; ==============================================================
;; KEY BINDINGS
;; ==============================================================

(global-set-key (kbd "M-v")        #'my/scroll-half-page-down)
(global-set-key (kbd "C-v")        #'my/scroll-half-page-up)

(global-set-key (kbd "<f5>")       #'my/revert-buffer)
(global-set-key (kbd "<f6>")       #'my/kill-asterisk-buffers)
(global-set-key (kbd "<f9>")       #'minimap-mode)
(global-set-key (kbd "<f12>")      #'xref-find-definitions)

(global-set-key (kbd "<prior>")    #'drag-stuff-up)
(global-set-key (kbd "<next>")     #'drag-stuff-down)

(global-set-key (kbd "C-x 0")      #'kill-buffer-and-window)
(global-set-key (kbd "C-c d")      #'my/duplicate-current-line-or-region)
(global-set-key (kbd "C-c k")      #'kill-whole-line)
(global-set-key (kbd "C-c l")      #'my/kill-word-at-point)
(global-set-key (kbd "C-c s")      #'my/kill-sentence-at-point)
(global-set-key (kbd "C-c x")      #'delete-trailing-whitespace)
(global-set-key (kbd "C-c w")      #'my/toggle-highlight-trailing-whitespaces)
(global-set-key (kbd "C-c h")      #'my/toggle-idle-highlight-mode)
(global-set-key (kbd "C-c C-e")    #'eval-region)
(global-set-key (kbd "C-c t")      #'my/untabify-entire-buffer)

(global-set-key (kbd "C-c o i")    #'my/open-init-file)
(global-set-key (kbd "C-c o a")    #'org-agenda-list)

(global-set-key (kbd "C-c p r")    #'helm-projectile-recentf)
(global-set-key (kbd "C-c p R")    #'projectile-replace)
(global-set-key (kbd "C-c p x")    #'projectile-replace-regexp)
(global-set-key (kbd "C-,")        #'helm-projectile-grep)
(global-set-key (kbd "C-.")        #'helm-projectile-ag)

(define-key helm-map (kbd "TAB")   #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   #'helm-select-action)

(global-set-key (kbd "C-,")        #'helm-projectile-grep)
(global-set-key (kbd "C-.")        #'helm-projectile-ag)

(define-key org-mode-map (kbd "C-x C-z")  #'outline-hide-entry)
(define-key org-mode-map (kbd "C-x C-a")  #'outline-hide-body)
(define-key org-mode-map (kbd "C-x C-n")  #'outline-next-heading)
(define-key org-mode-map (kbd "C-x C-p")  #'outline-prev-heading)

(define-key org-agenda-mode-map (kbd "m")  #'org-agenda-month-view)

(eval-after-load 'verilog-mode
  '(define-key verilog-mode-map (kbd "C-{") 'verilog-beg-of-defun))

(eval-after-load 'verilog-mode
  '(define-key verilog-mode-map (kbd "C-}") 'verilog-end-of-defun))

;; ==============================================================
;; HOOKS
;; ==============================================================

;; PROG
(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; POST COMMAND
;; (add-hook 'post-command-hook #'highlight-syntax-duplicates)

;; KILL BUFFER / QUIT WINDOW
;; (add-hook 'kill-buffer-hook <fun>)
;; (add-hook 'quit-window-hook <fun>)

;; XREF
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; MINIBUFFER
(defun my/minibuffer-setup ()
  "Function sets font size in the minibuffer"
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.3))))

(add-hook 'minibuffer-setup-hook 'my/minibuffer-setup)

(message "... finished reading ~/.emacs.d/init.el")
