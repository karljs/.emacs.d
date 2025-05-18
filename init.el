(require 'package)
(setq package-enable-at-startup nil)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/")
 t)
(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


(use-package emacs
  :demand t
  :init
  (setq gc-cons-percentage 0.5
        gc-cons-threshold (* 128 1024 1024))

  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (set-face-attribute 'default nil
		      :font "PragmataPro"
		      :height 170)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none)
  (tool-bar-mode -1)
  (set-scroll-bar-mode nil)
  (setq help-window-select t)
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq-default indent-tabs-mode nil)
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))

  ;; (load-theme 'modus-operandi t)

  (setq major-mode-remap-alist
        '(
          ;; (yaml-mode . yaml-ts-mode)
          ;; (bash-mode . bash-ts-mode)
          ;; (js2-mode . js-ts-mode)
          ;; (typescript-mode . typescript-ts-mode)
          ;; (json-mode . json-ts-mode)
          ;; (css-mode . css-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)))

  (eval-and-compile
    (mapc #'(lambda (entry)
              (define-prefix-command (cdr entry))
              (bind-key (car entry) (cdr entry)))
          '(
            ;; ("C-,"   . my-ctrl-comma-map)
            ;; ("<C-m>" . my-ctrl-m-map)
            ("C-h e" . my-emacs-lisp-help-map)
            ;; ("C-c b" . my-bookmarks-bibliography-map)
            ;; ("C-c e" . my-emacs-lisp-map)
            ;; ("C-c m" . my-ctrl-c-m-map)
            ;; ("C-c n" . my-ctrl-c-n-map)
            ;; ("C-c t" . my-multi-term-map)
            ;; ("C-c w" . my-web-map)
            ;; ("C-c y" . my-yasnippet-map)
            ;; ("C-c H" . my-highlight-map)
            ;; ("C-c N" . my-ctrl-c-N-map)
            ))))


(use-package no-littering
  :ensure
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
	`(("." . ,(no-littering-expand-var-file-name "backups/")))))


(use-package exec-path-from-shell
  :ensure
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; (use-package lsp-mode
;;   :ensure)

;; (use-package lsp-ui
;;   :ensure
;;   :after lsp-mode)

;; (use-package lsp-treemacs
;;   :ensure
;;   :after lsp-mode)

;; (use-package dap-mode
;;   :ensure
;;   :after lsp-mode)

(use-package eglot
  :ensure
  :after cc-mode
  :hook
  (c-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  :bind (("C-c C-a" . eglot-code-actions)))

(use-package consult-eglot
  :ensure
  :after eglot consult
  :bind (("C-c C-s" . consult-eglot-symbols)))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun kjs-ts-url (proj)
    (concat
     "https://github.com/tree-sitter/tree-sitter-"
     proj))
  (defun kjs-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             `((c . (,(kjs-ts-url "c") "v0.23.5"))
               (cpp . (,(kjs-ts-url "cpp") "v0.23.4"))
               (css . (,(kjs-ts-url "css") "v0.20.0"))
               (go . (,(kjs-ts-url "go") "v0.20.0"))
               (html . (,(kjs-ts-url "html") "v0.20.1"))
               (javascript . (,(kjs-ts-url "javascript") "v0.20.1" "src"))
               (json . (,(kjs-ts-url "json") "v0.20.2"))
               (markdown . (,(kjs-ts-url "markdown") "v0.7.1"))
               (python . (,(kjs-ts-url "python") "v0.20.4"))
               (rust . (,(kjs-ts-url "rust") "v0.21.2"))
               (toml . (,(kjs-ts-url "toml") "v0.5.1"))
               (tsx . (,(kjs-ts-url "typescript") "v0.20.3" "tsx/src"))
               (typescript . (,(kjs-ts-url "typescript") "v0.20.3" "typescript/src"))
               (yaml . (,(kjs-ts-url "yaml") "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  :config
  (kjs-setup-install-grammars))


(use-package vertico
  :ensure
  :init
  (vertico-mode))


(use-package orderless
  :ensure
  :config
  (setq orderless-matching-styles '(orderless-literal orderless-initialism orderless-flex)
	completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )


(use-package helpful
  :ensure
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))


(use-package corfu
  :ensure
  :init
  (global-corfu-mode)
  :config
  (setq tab-always-indent 'complete))


(use-package smartparens
  :ensure
  :hook
  (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config)
  :bind (("C-c <left>" . sp-forward-barf-sexp)
         ("C-c <right>" . sp-forward-slurp-sexp)
         ("C-c S-<left>" . sp-backward-slurp-sexp)
         ("C-c S-<right>" . sp-backward-barf-sexp)
         ("C-M-t" . sp-transpose-sexp)
         ("C-S-k" . sp-kill-hybrid-sexp)
         ("C-c C-<right>" . sp-slurp-hybrid-sexp)
         ("C-(" . sp-rewrap-sexp)
         ("C-M-<backspace>" . sp-splice-sexp-killing-around)))


(use-package doom-modeline
  :ensure
  :hook (after-init . doom-modeline-mode)
  :config
  ;; (setq doom-modeline-icon nil)
  (setq nerd-icons-font-family "PragmataPro")
)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))


(use-package avy
  :ensure
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char))


(use-package ace-window
  :ensure
  :bind ("M-o". ace-window))


(use-package change-inner
  :ensure
  :bind (("M-i" . change-inner)))


(use-package magit
  :ensure
  :bind (
	 ("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 ("C-c M-g" . magit-file-dispatch)))


(use-package projectile
  :ensure
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(use-package rust-mode
  :ensure
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  ;; (add-hook 'rust-mode-hook #'lsp)
)


(use-package geiser
  :ensure)


(use-package geiser-racket
  :ensure
  :after geiser
  :config
  (setq geiser-racket-binary "/usr/local/bin/racket"))


(use-package cc-mode
  :config
  ;; (electric-indent-mode -1)

  ;; Borrowed from the LLVM project, except make it 4 space indent
  (defun llvm-lineup-statement (langelem)
    (let ((in-assign (c-lineup-assignments langelem)))
      (if (not in-assign)
          '++
        (aset in-assign 0
              (+ (aref in-assign 0)
                 (* 4 c-basic-offset)))
        in-assign)))
  ;; :hook
  ;; ((c-mode c++-mode) . (lambda ()
  ;;       		 (c-toggle-electric-state -1)))
)


(use-package realgud
  :ensure
  :defer)

(use-package realgud-lldb
  :ensure
  :defer
  :after realgud)

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

(use-package jinx
  :ensure
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package vterm
  :ensure
  :bind (("C-c t" . vterm)))

(use-package slime
  :ensure
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package vundo
  :ensure)
