;;; package --- Summary
;;; Commentary:

;;; Code:

;; detach customizations to a seperate file
(setq custom-file (locate-user-emacs-file "custom.el"))

(defvar use-xah-fly-keys nil
  "Toggle xah-fly-keys instead of evil.")

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; "sane defaults" - https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(setq delete-old-versions -1)		; delete excess backup versions silently
(setq version-control t)		; use version control
(setq vc-make-backup-files t)		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory (file-name-as-directory "backups"))))) ; which directory to put backups file
(setq vc-follow-symlinks t)				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory (file-name-as-directory "auto-save-list")) t))) ;transform backups file name
(setq inhibit-startup-screen t)	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)	; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq-default fill-column 80)		; toggle wrapping text at the 80th character
(save-place-mode)			  ; save position in files
(show-paren-mode)			  ; show matching delimiter

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Load EXWM
(use-package exwm
  :when (or (string= (getenv "DESKTOP_SESSION") "ubuntu-exwm") (string= (getenv "DESKTOP_SESSION") "exwm-gnome-flashback-session"))
  :ensure t
  :custom
  (exwm-input-simulation-keys
   '(("" .
      [left])
     ("" .
      [right])
     ("" .
      [up])
     ("" .
      [down])
     ("" .
      [home])
     ("" .
      [end])
     ([134217846]
      .
      [prior])
     ("" .
      [next])
     ("" .
      [delete])
     ("" .
      [S-end delete])))
  :config
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)
  ;; remove IDO
  (ido-mode -1)
  (remove-hook 'exwm-init-hook #'exwm-config--fix/ido-buffer-window-other-frame)
  ;; (push ?\M-\  exwm-input-prefix-keys)
  ;; (push ?\C-\g exwm-input-prefix-keys)
  ;; (push ?\C-\\ exwm-input-prefix-keys)
  ;; (push ?\  exwm-input-prefix-keys) ; for leader key
  ;; hack around Ediff opening frames and messing up
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (require 'exwm-randr)
  (exwm-randr-enable))

(defun change-to-he ()
  "Change to hebrew."
  (interactive)
  (exwm-input-set-local-simulation-keys
   '(([?a] . [169 215]))))

;; Quelpa
(use-package quelpa
  :ensure t
  :init (setq quelpa-update-melpa-p nil))

;; Quelpa handler for `use-package'
(use-package quelpa-use-package
  :ensure t)

;; Vim mode
(use-package evil
  :unless use-xah-fly-keys
  :ensure t
  :custom
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil)
  ;; finer `undo' while in insert mode
  (evil-want-fine-undo t)
  (evil-move-cursor-back nil)
  (evil-move-beyond-eol t)
  :config
  (evil-mode 1)
  ;; Taken from: https://gist.github.com/kidd/d8dcf0d40d184ecec0a87d28cee52a49
  (cl-loop for (mode . state) in '(;; (inferior-emacs-lisp-mode      . emacs)
				   ;; (pylookup-mode                 . emacs)
				   ;; (navi-mode                     . emacs)
				   ;; (archive-mode                  . emacs)
				   ;; (comint-mode                   . emacs)
				   ;; (ebib-entry-mode               . emacs)
				   ;; (dirtree-mode                  . emacs)
				   ;; (image-mode                    . emacs)
				   ;; (ebib-log-mode                 . emacs)
				   ;; (gtags-select-mode             . emacs)
				   ;; (shell-mode                    . emacs)
				   ;; (term-mode                     . emacs)
				   ;; (bc-menu-mode                  . emacs)
				   ;; (grep-mode                     . emacs)
				   ;; (help-mode                     . emacs)
				   ;; (eww-mode                      . emacs)
				   ;; (google-contacts-mode          . emacs)
				   ;; (magit-branch-manager-mode-map . emacs)
				   ;; (semantic-symref-results-mode  . emacs)
				   ;; (fundamental-mode              . emacs)
				   ;; (cider-stacktrace-mode         . emacs)
				   ;; (diff-mode                     . emacs)
				   ;; (magit-popup-mode              . emacs)
				   ;; (magit-revision-mode           . emacs)
				   ;; (magit-mode                    . emacs)
				   ;; (magit-refs-mode               . emacs)
				   ;; (git-rebase-mode               . emacs)
				   ;; (magit-popup-sequence-mode     . emacs)
				   ;; (git-rebase-mode               . emacs)
				   ;; (elfeed-show-mode              . emacs)
				   ;; (elfeed-search-mode            . emacs)
				   ;; (docker-images-mode            . emacs)
				   ;; (docker-containers-mode        . emacs)
				   ;; (cider-docview-mode            . emacs)
				   (sly-db-mode                   . emacs)
				   (sly-inspector-mode            . emacs)
				   (sly-trace-dialog-mode         . emacs)
				   (sly-stickers--replay-mode     . emacs)
				   (sly-xref-mode                 . emacs)
				   ;; (notmuch-tree-mode             . emacs)
				   ;; (magit-repolist-mode           . emacs)
				   ;; (xref--xref-buffer-mode        . emacs)
				   ;; (eww-mode                      . emacs)
				   ;; (neotree-mode                  . emacs)
				   (sly-connection-list-mode      . emacs)
				   (sly-thread-control-mode       . emacs)
				   ;; (rdictcc-buffer-mode           . emacs)
				   )
	   do (evil-set-initial-state mode state)))

;; `evil' integration with other packages
(use-package evil-collection
  ;; :after evil-surround
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-outline-bind-tab-p nil)
  ;; (evil-collection-company-use-tng nil)
  :config
  ;; (require 'evil-surround)
  ;; (require 'lispyville)
  ;; (setq evil-collection-mode-list (seq-filter (lambda (elt)
  ;; 						(not (if (listp elt)
  ;; 							 (member 'ztree elt)
  ;; 						       (equal elt 'ztree))))
  ;; 					      evil-collection-mode-list))
  (evil-collection-init)
  ;; mimick `company-tng-configure-default', but don't disable choosing
  ;; (with-eval-after-load 'company
  ;;   (setq company-require-match nil)
  ;;   (setq company-frontends '(;; company-tng-frontend
  ;;                             company-pseudo-tooltip-frontend
  ;;                             company-echo-metadata-frontend))
  ;;   (let ((keymap company-active-map))
  ;;     (define-key keymap [tab] 'company-select-next)
  ;;     (define-key keymap (kbd "TAB") 'company-select-next)
  ;;     (define-key keymap [backtab] 'company-select-previous)
  ;;     (define-key keymap (kbd "S-TAB") 'company-select-previous))
  ;;   )
  )

;; Vim mode surround functionality
(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Theme
(use-package doom-themes
  :disabled
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package dracula-theme
  :config (load-theme 'dracula t)
  :ensure t)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; avy
(use-package avy
  :unless use-xah-fly-keys
  :ensure t
  :commands (avy-goto-word-1))

;; Smex (for ivy?)
(use-package smex
  :ensure t)

;; ivy
(use-package ivy
  :ensure t
  ;; :commands (ivy-switch-buffer)
  :after smex
  :custom
  (ivy-mode 1)
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

;; counsel
(use-package counsel
  :ensure t
  :config (counsel-mode)
  ;; :commands (counsel-M-x counsel-find-file counsel-describe-variable counsel-describe-function)
  )

;; swiper
;; (use-package swiper
;;   :after ivy
;;   :ensure t
;;   ;; :commands (swiper)
;;   )

;; Company mode
(use-package company
  :ensure t
  :init
  ;; No delay in showing suggestions.
  (setq company-idle-delay 0)
  ;; Show suggestions after entering one character.
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  :hook (after-init . global-company-mode))

(use-package company-quickhelp
  :disabled
  :ensure t
  :after company
  :config
  (company-quickhelp-mode)
  ;; (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  )

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode))

;; Custom keybinding
(use-package general
  :unless use-xah-fly-keys
  :ensure t
  :custom
  (general-override-states '(insert
                             emacs
                             hybrid
                             normal
                             visual
                             motion
                             operator
                             replace))
  :config
  (general-define-key
   ;; replace default keybindings
   "C-s" 'swiper-isearch ; search for string in current buffer
   "M-x" 'counsel-M-x            ; replace default M-x with ivy backend
   ;; "TAB" 'indent-for-tab-command ; replace default with 'smart' one
   "s-!" 'counsel-linux-app ; for EXWM
   "C->" 'eyebrowse-next-window-config
   "C-<" 'eyebrowse-prev-window-config)
  (general-define-key
   :states '(normal visual motion emacs)
   :prefix "C-w"
   "M-c" 'eyebrowse-create-window-config
   "M-r" 'eyebrowse-rename-window-config)
  (general-define-key
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
   "TAB" '(spacemacs/alternate-buffer :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "P" 'counsel-yank-pop ; kill ring
   ;; Jumping
   "j" '(:ignore t :which-key "jump")
   "jw" '(avy-goto-word-1 :which-key "word")
   "jl" '(link-hint-open-link :which-key "link")
   ;; Files
   "f" '(:ignore t :which-key "files")
   "ff" '(counsel-find-file :which-key "find files")
   "fe" '(find-user-init-file :which-key "init file")
   ;; Buffers
   "b" '(:ignore t :which-key "buffers")
   "bb" '(ivy-switch-buffer :which-key "switch buffer")
   "b0" '(kill-buffer-and-window :which-key "kill buffer and window")
   ;; Window
   "w" '(:ignore t :which-key "windows")
   "wl"  '(windmove-right :which-key "move right")
   "wh"  '(windmove-left :which-key "move left")
   "wk"  '(windmove-up :which-key "move up")
   "wj"  '(windmove-down :which-key "move bottom")
   "w/"  '(split-window-right :which-key "split right")
   "w-"  '(split-window-below :which-key "split bottom")
   "w0"  '(delete-window :which-key "delete window")
   "w1"  '(delete-other-windows :which-key "maximize window")
   ;; Quit
   "q" '(:ignore t :which-key "quit")
   "qq" '(save-buffers-kill-terminal :which-key "quit client")
   ;; Help
   "h" '(:ignore t :which-key "help")
   "hv" '(counsel-describe-variable :which-key "describe variable")
   "hf" '(counsel-describe-function :which-key "describe function")
   "hk" '(describe-key :which-key "describe key")
   "hc" '(describe-key-briefly :which-key "show key binding")
   "hb" '(counsel-descbinds :which-key "describe bindings")
   ;; Projectile
   "p" '(projectile-command-map :which-key "projects")
   ;; Applications
   "a" '(:ignore t :which-key "apps")
   "at"  '(ansi-term :which-key "open terminal")
   ;; git
   "g" '(:ignore t :which-key "git")
   "gs" '(magit-status :which-key "status")))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  (go-mode . (lambda () (setq tab-width 4)))
  ;; (go-mode . (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil t)))
  (lsp-after-open . (lambda ()
		      (remove-hook 'before-save-hook #'gofmt-before-save t)
		      (add-hook 'before-save-hook #'gofmt-before-save nil t)))
  ;; :custom
  ;; (gofmt-command "goimports")
  )

(use-package go-eldoc
  :ensure t
  :after go-mode
  :commands go-eldoc-setup
  :hook (go-mode . go-eldoc-setup))

(use-package go-dlv
  :ensure t
  :after go-mode
  :commands (dlv dlv-current-func)
  :custom
  (go-dlv-command-name "~/twistlock/dlv --listen=localhost:23456"))

(use-package lsp-mode
  ;; :quelpa (lsp-mode :fetcher github :repo "farva/lsp-mode" :branch "redundant-client")
  :ensure t
  :commands (lsp lsp-deferred)
  ;; :init
  ;; (setq lsp-clients-go-server "")
  ;; (setq lsp-clients-go-server "bingo")
  ;; disable lsp-eldoc
  ;; (add-hook 'go-mode-hook (lambda ()
  ;; 			    (lsp)
  ;; 			    (remove-hook 'before-save-hook 'lsp--before-save t)
  ;; 			    (add-hook 'before-save-hook #'lsp--before-save t t))
  ;; 	    t)
  ;; :hook (go-mode . (lambda ()
  ;; 		     (lsp)
  ;; 		     (remove-hook 'before-save-hook 'lsp--before-save t)
  ;; 		     (add-hook 'before-save-hook #'lsp--before-save t t)))
  :custom
  ;; (lsp-clients-go-server-args '("--diagnostics-style"
  ;; 				"instant"
  ;; 				"--format-style"
  ;; 				"goimports"))
  (lsp-prefer-flymake nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-enable-signature-help nil)
  (lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
  :hook
  (go-mode . lsp-deferred)
  (sh-mode . lsp-deferred)
  (html-mode . lsp-deferred)
  (js-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (c-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "bingo --diagnostics-style=instant")
  ;;                   :major-modes '(go-mode)
  ;;                   :priority 1
  ;;                   :server-id 'my-go-bingo
  ;;                   :library-folders-fn (lambda (_workspace)
  ;;                                         lsp-clients-go-library-directories)))
  ;; (defun my:lsp-clients-go--make-init-options ()
  ;;   "My init options for go-langserver."
  ;;   `(:funcSnippetEnabled ,(lsp-clients-go--bool-to-json lsp-clients-go-func-snippet-enabled)
  ;;                         :gocodeCompletionEnabled ,(lsp-clients-go--bool-to-json lsp-clients-go-gocode-completion-enabled)
  ;;                         :formatTool ,lsp-clients-go-format-tool
  ;; 			  :lintTool "golint"
  ;;                         :goimportsLocalPrefix ,lsp-clients-go-imports-local-prefix
  ;;                         :maxParallelism ,lsp-clients-go-max-parallelism
  ;;                         :useBinaryPkgCache ,lsp-clients-go-use-binary-pkg-cache
  ;;                         :diagnosticsEnabled ,(lsp-clients-go--bool-to-json lsp-clients-go-diagnostics-enabled)))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "gopls")
  ;;                   :major-modes '(go-mode)
  ;;                   :priority -10
  ;; 		    :server-id 'gopls))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection
  ;; 				     (lambda () (cons "gopls" lsp-gopls-server-args)))
  ;; 		    :major-modes '(go-mode)
  ;; 		    :remote? t
  ;; 		    :priority 10
  ;; 		    :server-id 'gopls-remote
  ;; 		    :library-folders-fn (lambda (_workspace)
  ;; 					  lsp-clients-go-library-directories)))
  )

;; (use-package lsp-ui
;;   :ensure t
;;   ;; :hook (lsp-mode . lsp-ui-mode)
;;   :bind (:map lsp-ui-mode-map
;; 	      ([remap lsp-find-definition] . lsp-ui-peek-find-definitions)
;; 	      ([remap lsp-find-references] . lsp-ui-peek-find-references))
;;   ;; :commands lsp-ui
;;   ;; :custom
;;   ;; (lsp-ui-sideline-enable t)
;;   ;; (lsp-ui-sideline-ignore-duplicate t)
;;   ;; (lsp-ui-sideline-show-hover nil)
;;   ;; (lsp-ui-sideline-show-symbol nil)
;;   ;; :config
;;   ;; (with-eval-after-load 'evil
;;   ;;   (evil-define-key 'normal lsp-ui-mode-map
;;   ;;     (kbd "gd") #'xref-find-definitions
;;   ;;     (kbd "gr") #'xref-find-references))
;;   )

(use-package lsp-ui
  :ensure t
  ;; :commands lsp-ui-mode
  )

;; lsp-company should work out-of-the-box
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   :after company
;;   ;; :init (add-to-list 'company-backends #'company-lsp)
;;   )

(use-package lsp-ivy
  :after ivy
  :ensure t
  ;; :after lsp
  :commands lsp-ivy-workspace-symbol
  :bind (:map lsp-mode-map
	      ([remap xref-find-apropos] . lsp-ivy-workspace-symbol))
  ;; :hook (lsp-mode . (lambda () (evil-define-key 'normal lsp-mode-map
  ;; 				 (kbd "ga") lsp-ivy-workspace-symbol)))
  ;; :config
  ;; (with-eval-after-load 'lsp-mode
  ;;   (evil-define-key 'normal lsp-mode-map
  ;;     (kbd "ga") lsp-ivy-workspace-symbol))
  )

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package eglot
  :disabled
  :ensure t
  :hook (go-mode . eglot-ensure)
  ;; :init
  ;; (with-eval-after-load 'eglot
  ;;   (assq-delete-all 'go-mode eglot-server-programs)
  ;;   (add-to-list 'eglot-server-programs
  ;; 		 '(go-mode . ("bingo" "-mode=stdio"))))
  )

;; Projectile
(use-package projectile
  :ensure t
  :custom
  ;; (projectile-indexing-method 'native)
  (projectile-git-command
   "git ls-files -zc --exclude-standard | tr '\\0' '\\n' | grep -v node_modules | grep -v www/public/libs/ | grep -v vendor/ | grep -v POCs/ | grep -v nobu/ | grep -v pkg/test/test_container/ | tr '\\n' '\\0'")
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :after (:all counsel projectile)
  :config
  (counsel-projectile-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("twistlock/.*\\.md\\'" . gfm-mode)
	 ("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-header-scaling t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-code-lang-modes
   '(("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("screen" . shell-script-mode)
     ("console" . sh-mode)
     ("shellsession" . sh-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("go" . go-mode))))

(use-package gh-md
  :ensure t)

;; mmm
(use-package mmm-mode
  :disabled
  :ensure t
  :commands mmm-mode
  :hook markdown-mode
  :custom
  (mmm-parse-when-idle 't)
  :config
  ;; TODO refactor like https://jblevins.org/log/mmm
  (mmm-add-classes '((markdown-ini
                      :submode conf-unix-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *ini[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-python
                      :submode python-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *python[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-html
                      :submode web-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *html[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-java
                      :submode java-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *java[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-ruby
                      :submode ruby-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *ruby[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-c
                      :submode c-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *c[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-c++
                      :submode c++-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *c\+\+[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-elisp
                      :submode emacs-lisp-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *elisp[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-javascript
                      :submode javascript-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *javascript[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-ess
                      :submode R-mode
                      :face mmm-declaration-submode-face
                      :front "^```{?r.*}?[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-rust
                      :submode rust-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *rust[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-go
                      :submode go-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *go[\n\r]+"
                      :back "^```$")))
  (mmm-add-classes '((markdown-console
                      :submode shell-script-mode
                      :face mmm-declaration-submode-face
                      :front "^``` *\\(shellsession\\|console\\|shell\\)[\n\r]+"
                      :back "^```$")))
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-java)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ruby)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c++)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-elisp)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-html)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-javascript)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ess)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-rust)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ini)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-go)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-console))

;; GH live preview markdown
(use-package vmd-mode
  :ensure t
  :init (setq vmd-binary-path "~/.npm-global/bin/vmd"))

;; smartparens
(use-package smartparens
  :ensure t
  :unless use-xah-fly-keys
  :commands (smartparens-strict-mode smartparens-mode)
  :hook
  (sly-mrepl-mode . smartparens-strict-mode)
  (prog-mode . smartparens-mode)
  ;; :init (add-hook 'prog-mode-hook (lambda ()
  ;; 				    (if (derived-mode-p 'sh-mode)
  ;; 					(smartparens-mode)
  ;; 				      (smartparens-strict-mode))))
  :config
  (require 'smartparens-config))

;; lispy
;; (use-package lispy
;;   :after evil-collection
;;   :ensure t
;;   :hook (emacs-lisp-mode lisp-mode))

;; lispyvill
(use-package lispyville
  :after evil-surround
  :unless use-xah-fly-keys
  :ensure t
  :commands (lispyville-wrap-lispy-mark-symbol-visual
	     lispyville-wrap-lispy-mark-visual
	     lispyville-wrap-lispy-mark-symbol-special
	     lispyville-wrap-lispy-mark-special)
  :hook (prog-mode . lispyville-mode)
  :config
  (with-eval-after-load 'evil-commentary
    (add-to-list 'evil-commentary-comment-function-for-mode-alist '(emacs-lisp-mode . lispyville-comment-or-uncomment))
    (add-to-list 'evil-commentary-comment-function-for-mode-alist '(lisp-mode . lispyville-comment-or-uncomment))))

;; (use-package lispyville
;;   ;; :init
;;   ;; (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
;;   :hook lispy-mode
;;   :config
;;   (lispyville-set-key-theme '(operators c-w additional)))

;; REST-client
(use-package restclient
  :ensure t
  :commands restclient-mode)

;; Dashboard
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :config
  ;; (setq dashboard-banner-logo-title "your custom text")
  ;; (setq dashboard-startup-banner "/path/to/image")
  ;; (setq dashboard-items '((recents  . 10)
  ;;                         (bookmarks . 10)))
  (setq dashboard-startup-banner 'logo)
  ;; copy-pasted the logic of (dashboard-setup-startup-hook):
  (add-hook 'after-init-hook (lambda ()
			       ;; Display useful lists of items
			       (dashboard-insert-startupify-lists)))
  (add-hook 'emacs-startup-hook '(lambda ()
				   (switch-to-buffer "*dashboard*")
				   (goto-char (point-min))
				   (redisplay))))

;; Evil comments
(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Magit
(use-package magit
  :ensure t
  :custom (magit-git-executable "/usr/local/bin/git"))

;; Yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook
	    '(lambda ()
	       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Go test
(use-package gotest
  :ensure t
  :after go-mode)

;; Go Playground locally
(use-package go-playground
  :ensure t)

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; Elpy
(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  :config (elpy-enable))

;; Aggressive indent
(defun aggressive-indent-avoid-sly-scratch ()
  "Enable aggressive indenting but not in `sly-scratch' buffer."
  (if (and (fboundp 'sly-buffer-name)
	   (string= (sly-buffer-name :scratch) (buffer-name)))
      (electric-indent-local-mode -1)
    (aggressive-indent-mode)))

(use-package aggressive-indent
  :ensure t
  :hook
  (lisp-mode . aggressive-indent-avoid-sly-scratch)
  (emacs-lisp-mode . aggressive-indent-mode))

;; Workspaces
(use-package eyebrowse
  :ensure t
  :diminish eyebrowse-mode
  :config
  (eyebrowse-mode t)
  (setf (cdr eyebrowse-mode-map) nil))

;; ace link
(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

;; link-help
(use-package link-hint
  :ensure t
  :defer t)

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/zsh"))

;; backward-forward
(use-package backward-forward
  :ensure t
  :config
  ;; TODO manage dependency with Evil
  (setf backward-forward-evil-compatibility-mode t)
  (backward-forward-mode t))

;; highlight indent
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-method 'character))

;; JSCS
;; (use-package jscs
;;   :ensure t
;;   :after js2-mode
;;   :hook
;;   (js-mode . jscs-indent-apply)
;;   (js2-mode . jscs-indent-apply)
;;   (json-mode . jscs-indent-apply)
;;   ;; on save
;;   (js-mode . jscs-fix-run-before-save)
;;   (js2-mode . jscs-fix-run-before-save))

;; EXWM-evil
(use-package exwm-firefox-core
  :after exwm
  :ensure t)

(use-package exwm-firefox-evil
  :after exwm-firefox-core
  :ensure t
  :commands exwm-firefox-evil-mode
  :hook (exwm-manage-finish . (lambda () (exwm-firefox-evil-mode 1)))
  :config
  ;; save `<escape>' from the jaws of the `insert' mode
  (push 'escape exwm-input-prefix-keys)
  ;; hijack `C-g'
  (push ?\C-\g exwm-input-prefix-keys)

  ;; override `normal' function
  (defun my:exwm-firefox-evil-normal ()
    "Pass every key directly to Emacs."
    (interactive)
    ;; (setq-local exwm-input-line-mode-passthrough t)
    (evil-force-normal-state)
    (unless (local-variable-p 'exwm-input-prefix-keys)
      (make-local-variable 'exwm-input-prefix-keys)
      (let ((keys '(?~ ?\` ?\; ?1 ?! ?2 ?@ ?3 ?\# ?4 ?$ ?5 ?% ?6 ?^ ?7 ?& ?8 ?* ?9 ?\( ?0 ?\) ?- ?_ ?= ?+ ?q ?Q ?w ?W ?e ?E ?r ?R ?t ?T ?y ?Y ?u ?U ?i ?I ?o ?O ?p ?P ?\[ ?\] ?\{ ?\} ?\\ ?\| ?a ?A ?s ?S ?d ?D ?f ?F ?g ?G ?h ?H ?j ?J ?k ?K ?l ?L ?\; ?: ?\' ?\" ?z ?Z ?x ?X ?c ?C ?v ?V ?b ?B ?n ?N ?m ?M ?\, ?< ?\. ?> ?/ ?? ?\ ?\
		       ?\C-s ?\t)))
	(dolist (elt keys)
	  (push elt exwm-input-prefix-keys)))))
  ;; (define-key exwm-firefox-evil-mode-map [remap evil-force-normal-state] 'my:exwm-firefox-evil-normal)
  (advice-add 'exwm-firefox-evil-normal :override #'my:exwm-firefox-evil-normal)

  ;; override `insert' function
  (defun my:exwm-firefox-evil-insert (;; count &optional vcount skip-empty-lines
				      )
    "Pass every key to firefox."
    (interactive
     ;; (list (prefix-numeric-value current-prefix-arg)
     ;;       (and (evil-visual-state-p)
     ;; 		(memq (evil-visual-type) '(line block))
     ;; 		(save-excursion
     ;;              (let ((m (mark)))
     ;;                ;; go to upper-left corner temporarily so
     ;;                ;; `count-lines' yields accurate results
     ;;                (evil-visual-rotate 'upper-left)
     ;;                (prog1 (count-lines evil-visual-beginning evil-visual-end)
     ;;                  (set-mark m)))))
     ;;       (evil-visual-state-p))
     )
    ;; (setq-local exwm-input-line-mode-passthrough nil)
    ;; (evil-insert count vcount skip-empty-lines)
    (evil-insert-state)
    (kill-local-variable 'exwm-input-prefix-keys))
  ;; (define-key exwm-firefox-evil-mode-map [remap evil-insert] 'my:exwm-firefox-evil-insert)
  (advice-add 'exwm-firefox-evil-insert :override #'my:exwm-firefox-evil-insert)

  ;; Find
  (evil-define-key 'normal exwm-firefox-evil-mode-map (kbd "C-s") 'exwm-firefox-core-find))

(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "ros -Q run")
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal sly-mode-map
      (kbd "gd") #'sly-edit-definition
      (kbd "gr") #'sly-edit-uses)))

;;(use-package pdf-tools
;;  :ensure t
;;  :config
;;  (pdf-loader-install)
;;  (defun me/pdf-set-last-viewed-bookmark ()
;;    (interactive)
;;    (when (eq major-mode 'pdf-view-mode)
;;      (bookmark-set (me//pdf-get-bookmark-name))))
;;
;;  (defun me//pdf-jump-last-viewed-bookmark ()
;;    (bookmark-set "fake")
;;    (let ((bmk (me//pdf-get-bookmark-name)))
;;      (when (me//pdf-has-last-viewed-bookmark bmk)
;;        (bookmark-jump bmk))))
;;
;;  (defun me//pdf-has-last-viewed-bookmark (bmk)
;;    (assoc bmk bookmark-alist))
;;
;;  (defun me//pdf-get-bookmark-name ()
;;    (concat "PDF-last-viewed: " (buffer-file-name)))
;;
;;  (defun me//pdf-set-all-last-viewed-bookmarks ()
;;    (dolist (buf (buffer-list))
;;      (with-current-buffer buf (me/pdf-set-last-viewed-bookmark))))
;;
;;  (add-hook 'kill-buffer-hook 'me/pdf-set-last-viewed-bookmark)
;;  (add-hook 'pdf-view-mode-hook 'me//pdf-jump-last-viewed-bookmark)
;;
;;  ;; As `save-place-mode' does
;;  (unless noninteractive
;;    (add-hook 'kill-emacs-hook #'me//pdf-set-all-last-viewed-bookmarks)))

;; Docker TRAMP
(use-package docker-tramp
  :ensure t)

;; JS
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :init
  (setq-default js2-additional-externs
		'("$"
		  "$A" ; salesforce lightning component
		  "$LightningApp" ; salesforce
		  "AccessifyHTML5"
		  "Blob"
		  "FormData"
		  "KeyEvent"
		  "Raphael"
		  "React"
		  "URLSearchParams"
		  "__dirname" ; Node
		  "_content" ; Keysnail
		  "after"
		  "afterEach"
		  "angular"
		  "app"
		  "assert"
		  "assign"
		  "before"
		  "beforeEach"
		  "browser"
		  "by"
		  "clearInterval"
		  "clearTimeout"
		  "command" ; Keysnail
		  "content" ; Keysnail
		  "decodeURI"
		  "define"
		  "describe"
		  "display" ; Keysnail
		  "documentRef"
		  "element"
		  "encodeURI"
		  "expect"
		  "ext" ; Keysnail
		  "fetch"
		  "gBrowser" ; Keysnail
		  "global"
		  "goDoCommand" ; Keysnail
		  "hook" ; Keysnail
		  "inject"
		  "isDev"
		  "it"
		  "jQuery"
		  "jasmine"
		  "key" ; Keysnail
		  "ko"
		  "log"
		  "mockStore"
		  "module"
		  "mountWithTheme"
		  "plugins" ; Keysnail
		  "process"
		  "require"
		  "setInterval"
		  "setTimeout"
		  "shell" ; Keysnail
		  "tileTabs" ; Firefox addon
		  "util" ; Keysnail
		  "utag")))

;; Visualize color codes
(use-package rainbow-mode
  :ensure t
  :hook js2-mode)

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "GOBIN")
  (setenv "GOOS" "linux"))

;; (use-package todo-projectile
;;   :ensure t)

(use-package dap-mode
  :ensure t
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-go))

(use-package vterm
  :after evil
  :load-path "~/emacs-libvterm"
  :config
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq-local evil-insert-state-cursor 'box)
	      (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-libvterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-libvterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-libvterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  :config (setq rust-format-on-save t))

(use-package toml-mode
  :ensure t)

(use-package flycheck-rust
  :requires (rust-mode flycheck)
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup)
  :commands flycheck-rust-setup
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package org
  :ensure t
  :if (file-exists-p "~/org/init.el")
  :config (load "~/org/init.el"))

(use-package org-contrib
  :ensure t
  :after org)

;; Should be used when velocity will be kicked out of contrib
(use-package org-velocity
  :disabled
  :after org
  :quelpa (org-velocity :fetcher github-ssh :repo "ruricolist/org-velocity"))

(use-package org-velocity
  :after org-contrib)

(use-package helm-org-rifle
  :ensure t)

(use-package xah-fly-keys
  :when use-xah-fly-keys
  :quelpa (xah-fly-keys :fetcher github-ssh :repo "xahlee/xah-fly-keys")
  :custom
  (xah-fly-use-control-key nil)
  (xah-fly-use-meta-key nil)
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys)
  (with-eval-after-load 'counsel
    (setq xah-fly-M-x-command #'counsel-M-x)
    (general-emacs-define-key xah-fly-command-map [remap isearch-forward] #'swiper-isearch)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions and stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-user-init-file ()
  "Open the Emacs init file in the current window."
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

(defun flash-mode-line ()
  "Flash mode line."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current WINDOW."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window)))
     nil t)))

;; https://www.reddit.com/r/emacs/comments/8jaflq/tip_how_to_use_your_dashboard_properly/
(defun my/dashboard-banner ()
  "Set a dashboard banner including information on package initialization time and garbage collections."
  (setq dashboard-banner-logo-title
        (format "Emacs ready in %.2f seconds: %d packages activated; %d garbage collections."
                (float-time (time-subtract after-init-time before-init-time)) (length package-activated-list) gcs-done)))

;; Swiper with symbol at point
(defun swiper-at-point ()
  "Call `counsel-grep-or-swiper' with symbol at point as initial value."
  (interactive)
  (counsel-grep-or-swiper
   (when-let ((sym (symbol-at-point)))
     (symbol-name sym))))

;; (which-function-mode)
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;       ;; We remove Which Function Mode from the mode line, because it's mostly
;;       ;; invisible here anyway.
;;       (assq-delete-all 'which-func-mode mode-line-misc-info))

(define-derived-mode go-test-mode-as-root compilation-mode "Go-Test."
  "Major mode for the Go-Test compilation buffer."
  (use-local-map go-test-mode-map)
  (setq major-mode 'go-test-mode)
  (setq mode-name "Go-Test")
  (setq-local truncate-lines t)
  ;;(run-hooks 'go-test-mode-hook)
  (font-lock-add-keywords nil go-test-font-lock-keywords))

(defun go-test--go-test-as-root (args &optional env)
  "Start the go test command using `ARGS', using `ENV'."
  (let ((buffer "*Go Test*")) ; (concat "*go-test " args "*")))
    (go-test--cleanup buffer)
    (compilation-start (go-test--get-program (go-test--arguments args) " PATH=$PATH sudo -E")
                       t
                       'go-test--compilation-name)
    ;; (with-current-buffer "*Go Test*"
    ;;   (rename-buffer buffer)
    ;;   (go-test-mode-as-root))
    (set-process-sentinel (get-buffer-process buffer) 'go-test--finished-sentinel)))

;; (advice-add 'go-test--go-test :override #'go-test--go-test-as-root)

;; (when (eq system-type 'darwin)
;;   (setq mac-right-command-modifier 'meta)
;;   (setq mac-right-option-modifier 'control))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(defun run-goland-wrapper (f &rest args)
  "Run `F' with `ARGS' through the GoLand wrapper."
  (let ((go-command "GOROOT=~/twistlock/go1.12.7 ~/twistlock/go1.12.7/bin/go"))
    (apply f args)))
(advice-add 'go-test-current-test :around #'run-goland-wrapper)
(advice-add 'go-test-current-file :around #'run-goland-wrapper)
(advice-add 'go-test-current-project :around #'run-goland-wrapper)

;; Taken from: https://stackoverflow.com/a/20788581
(defun my-colorize-compilation-buffer ()
  "Compilation buffer with colors."
  (when (eq major-mode 'compilation-mode)
    (require 'ansi-color)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)

(defun my-rust-run ()
  "Build and run Rust code."
  (interactive)
  (let ((compilation-buffer-name-function (lambda (mode-name) "" "*Rust Run*")))
    (rust-run))
  (let (
	(orig-win (selected-window))
	(run-win (display-buffer (get-buffer "*Rust Run*") nil 'visible))
	)
    (select-window run-win)
    (comint-mode)
    (read-only-mode 0)
    (select-window orig-win)
    )
  )

(require 'ansi-color)
(defun display-ansi-colors ()
  "Display ANSI colors in buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;;;;;;;;;;;;;
;; kmacros ;;
;;;;;;;;;;;;;

(fset 'go-wrap-if-err
      [?^ ?i ?i ?f ?  ?e ?r ?r ?  ?! backspace ?: ?= ?  escape ?A ?\; ?  ?e ?r ?r ?  ?! ?= ?  ?n ?u ?i backspace backspace ?i ?l ?  ?\{ return ?r ?e ?t ?u ?r ?n ?  ?e ?r ?r return escape])

;; Load customizations from a dedicated file
(load custom-file)

(provide 'init)
;;; init.el ends here
