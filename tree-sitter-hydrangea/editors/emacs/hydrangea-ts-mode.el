;;; hydrangea-ts-mode.el --- Tree-sitter major mode for Hydrangea -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Hydrangea contributors
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Major mode for editing Hydrangea source files with Emacs tree-sitter support.

;;; Code:

(require 'prog-mode)
(require 'subr-x)

(eval-and-compile
  (unless (require 'treesit nil t)
    (error "hydrangea-ts-mode requires Emacs built with tree-sitter support")))

(defgroup hydrangea nil
  "Tree-sitter support for the Hydrangea language."
  :group 'languages)

(defcustom hydrangea-ts-mode-indent-offset 2
  "Indentation width used by `hydrangea-ts-mode'."
  :type 'integer
  :safe #'integerp
  :group 'hydrangea)

(defconst hydrangea-ts-mode--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `hydrangea-ts-mode.el'.")

(defconst hydrangea-ts-mode--repo-root
  (expand-file-name "../../.." hydrangea-ts-mode--directory)
  "Repository root for the Hydrangea checkout that ships this mode.")

(defun hydrangea-ts-mode--fontify-with-face (face _node override start end)
  "Apply FACE between START and END, respecting OVERRIDE."
  (treesit-fontify-with-override start end face override))

(defun hydrangea-ts-mode--fontify-number (node override start end &rest _)
  "Fontify numeric NODE between START and END."
  (hydrangea-ts-mode--fontify-with-face
   (if (facep 'font-lock-number-face)
       'font-lock-number-face
     'font-lock-constant-face)
   node override start end))

(defun hydrangea-ts-mode--fontify-operator (node override start end &rest _)
  "Fontify operator NODE between START and END."
  (hydrangea-ts-mode--fontify-with-face
   (if (facep 'font-lock-operator-face)
       'font-lock-operator-face
     'font-lock-builtin-face)
   node override start end))

(defun hydrangea-ts-mode--fontify-property (node override start end &rest _)
  "Fontify property NODE between START and END."
  (hydrangea-ts-mode--fontify-with-face
   (if (facep 'font-lock-property-name-face)
       'font-lock-property-name-face
     'font-lock-variable-name-face)
   node override start end))

(defvar hydrangea-ts-mode--font-lock-settings nil
  "Cached tree-sitter font-lock settings for Hydrangea.")

(defun hydrangea-ts-mode--font-lock-settings ()
  "Return tree-sitter font-lock settings for Hydrangea."
  (or hydrangea-ts-mode--font-lock-settings
      (setq hydrangea-ts-mode--font-lock-settings
            (treesit-font-lock-rules
             :language 'hydrangea
             :feature 'comment
             '((comment) @font-lock-comment-face)

             :language 'hydrangea
             :feature 'string
             '((string_literal) @font-lock-string-face)

             :language 'hydrangea
             :feature 'constant
             '((integer_literal) @hydrangea-ts-mode--fontify-number
               (float_literal) @hydrangea-ts-mode--fontify-number
               (boolean_literal) @font-lock-constant-face)

             :language 'hydrangea
             :feature 'keyword
             '(["let"
                "in"
                "if"
                "then"
                "else"
                "fn"
                "forall"
                "where"
                "bound"
                "dim"
                "elem"
                "All"
                "Any"
                "clamp"
                "wrap"
                "mirror"
                "constant"
                "generate"
                "fill"
                "replicate"
                "slice"
                "reshape"
                "map"
                "zipwith"
                "reduce"
                "reduce_generate"
                "foldl"
                "foldl_while"
                "scan"
                "scan_inclusive"
                "scanr"
                "scanr_inclusive"
                "segmented_reduce"
                "sort_indices"
                "iota"
                "make_index"
                "coo_sum_duplicates"
                "csr_from_sorted_coo"
                "permute"
                "scatter"
                "scatter_guarded"
                "gather"
                "sqrt"
                "fst"
                "snd"
                "expf"
                "log"
                "sin"
                "cos"
                "abs_f"
                "floor_f"
                "ceil_f"
                "erf"
                "float_of"
                "int_of_float"
                "proj"
                "index"
                "shape_of"
                "check_index"
                "read_array"
                "read_array_float"
                "write_array"
                "write_array_float"
                "get_env_int"
                "get_env_string"
                "stencil"] @font-lock-keyword-face)

             :language 'hydrangea
             :feature 'type
             '(["int" "bool" "float"] @font-lock-type-face
               (type_identifier (identifier) @font-lock-type-face)
               (polytype type_parameter: (identifier) @font-lock-type-face))

             :language 'hydrangea
             :feature 'definition
             '((declaration
                name: (identifier) @font-lock-function-name-face))

             :language 'hydrangea
             :feature 'property
             '((record_field
                name: (identifier) @hydrangea-ts-mode--fontify-property)
               (type_record_field
                name: (identifier) @hydrangea-ts-mode--fontify-property)
               (postfix_expression
                field: (identifier) @hydrangea-ts-mode--fontify-property))

             :language 'hydrangea
             :feature 'parameter
             '((variable_pattern
                name: (identifier) @font-lock-variable-name-face)
               (bound_pattern
                name: (identifier) @font-lock-variable-name-face))

             :language 'hydrangea
             :feature 'operator
             '(["+"
                "-"
                "*"
                "/"
                "%"
                "+."
                "-."
                "*."
                "/."
                "="
                "<>"
                "<"
                "<="
                ">"
                ">="
                "=."
                "<>."
                "<."
                "<=."
                ">."
                ">=."
                "=>"
                "->"
                "|"
                "&"] @hydrangea-ts-mode--fontify-operator)))))

(defvar hydrangea-ts-mode--font-lock-feature-list
  '((comment string)
    (constant keyword type)
    (definition property parameter operator))
  "Tree-sitter font-lock feature list for Hydrangea.")

(defvar hydrangea-ts-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\' "w" st)
    (modify-syntax-entry ?\? "w" st)
    st)
  "Syntax table used by `hydrangea-ts-mode'.")

(defun hydrangea-ts-mode--looking-at (bol regexp)
  "Return non-nil when BOL begins text matching REGEXP."
  (save-excursion
    (goto-char bol)
    (looking-at-p regexp)))

(defun hydrangea-ts-mode--line-starts-with (regexp &optional parent-types)
  "Build an indent matcher for REGEXP, optionally restricted to PARENT-TYPES."
  (lambda (node parent bol)
    (and (null node)
         (or (null parent-types)
             (and parent
                  (member (treesit-node-type parent) parent-types)))
         (hydrangea-ts-mode--looking-at bol regexp))))

(defun hydrangea-ts-mode--indent-rules ()
  "Return `treesit-simple-indent-rules' for Hydrangea."
  (let ((offset hydrangea-ts-mode-indent-offset))
    `((hydrangea
       ((hydrangea-ts-mode--line-starts-with
         ,(rx (* blank) (or "in" "then" "else") symbol-end)
         ("let_in_expression" "bound_let_expression" "conditional_expression"))
        parent-bol 0)
       ((hydrangea-ts-mode--line-starts-with
         ,(rx (* blank) (or ":" "=" (seq "where" symbol-end)))
         ("declaration"))
        parent-bol ,offset)
       ((hydrangea-ts-mode--line-starts-with
         ,(rx (* blank) (any ")" "]" "}"))
         ("pair_expression"
          "pair_pattern"
          "parenthesized_expression"
          "record_expression"
          "record_type"
          "shape_spec"
          "slice_spec"
          "vector_expression"
          "vector_pattern"))
        parent-bol 0)
       ((parent-is "source_file") column-0 0)
       ((match nil "declaration" "parameter" nil nil) parent-bol ,offset)
       ((match nil "declaration" "precondition" nil nil) parent-bol ,offset)
       ((match nil "declaration" "type" nil nil) parent-bol ,offset)
       ((match nil "declaration" "value" nil nil) parent-bol ,offset)
       ((match nil "let_in_expression" "binding" nil nil) parent-bol ,offset)
       ((match nil "let_in_expression" "body" nil nil) parent-bol ,offset)
       ((match nil "bound_let_expression" "value" nil nil) parent-bol ,offset)
       ((match nil "bound_let_expression" "body" nil nil) parent-bol ,offset)
       ((match nil "conditional_expression" "consequence" nil nil) parent-bol ,offset)
       ((match nil "conditional_expression" "alternative" nil nil) parent-bol ,offset)
       ((match nil "lambda_expression" "body" nil nil) parent-bol ,offset)
       ((match nil "application_expression" "argument" nil nil) parent-bol ,offset)
       ((match nil "binary_expression" "right" nil nil) parent-bol ,offset)
       ((parent-is
         "pair_expression\\|pair_pattern\\|parenthesized_expression\\|record_expression\\|record_type\\|shape_spec\\|slice_spec\\|vector_expression\\|vector_pattern")
        parent-bol ,offset)))))

(defun hydrangea-ts-mode--defun-name (node)
  "Return the declaration name for NODE."
  (when (equal (treesit-node-type node) "declaration")
    (when-let ((name-node (treesit-node-child-by-field-name node "name")))
      (treesit-node-text name-node t))))

(defun hydrangea-ts-mode--run-git (directory &rest args)
  "Run git with ARGS in DIRECTORY or signal an error."
  (with-temp-buffer
    (let ((default-directory directory))
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "git %s failed: %s"
               (string-join args " ")
               (string-trim (buffer-string)))))))

(defun hydrangea-ts-mode--prepare-local-grammar-repo (grammar-dir)
  "Stage GRAMMAR-DIR into a temporary git repository for installation."
  (let* ((staging-dir (make-temp-file "hydrangea-ts-mode-grammar" t))
         (repo-dir (expand-file-name "repo" staging-dir)))
    (make-directory repo-dir)
    (dolist (path '("grammar.js" "package.json" "queries" "src" "tree-sitter.json"))
      (let ((source (expand-file-name path grammar-dir))
            (target (expand-file-name path repo-dir)))
        (if (file-directory-p source)
            (copy-directory source target nil nil t)
          (copy-file source target nil t))))
    (hydrangea-ts-mode--run-git repo-dir "init" "--quiet")
    (hydrangea-ts-mode--run-git repo-dir "add" ".")
    (hydrangea-ts-mode--run-git
     repo-dir
     "-c" "user.name=Hydrangea"
     "-c" "user.email=hydrangea@example.invalid"
     "commit" "--quiet" "-m" "Temporary Hydrangea grammar snapshot")
    staging-dir))

;;;###autoload
(defun hydrangea-ts-mode-install-grammar ()
  "Install the Hydrangea tree-sitter grammar from the current checkout."
  (interactive)
  (let* ((local-grammar-dir
          (expand-file-name "tree-sitter-hydrangea" hydrangea-ts-mode--repo-root))
         (staging-dir
          (and (file-directory-p local-grammar-dir)
               (hydrangea-ts-mode--prepare-local-grammar-repo local-grammar-dir))))
    (unwind-protect
        (let ((treesit-language-source-alist
               `((hydrangea
                  ,@(if staging-dir
                        (list (expand-file-name "repo" staging-dir))
                      '("https://github.com/hydrangea-project/hydrangea-compiler"
                        nil
                        "tree-sitter-hydrangea"))))))
          (treesit-install-language-grammar 'hydrangea))
      (when staging-dir
        (delete-directory staging-dir t)))))

;;;###autoload
(define-derived-mode hydrangea-ts-mode prog-mode "Hydrangea"
  "Major mode for editing Hydrangea."
  :syntax-table hydrangea-ts-mode-syntax-table
  (setq-local comment-use-syntax nil)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\*+)")
  (setq-local indent-tabs-mode nil)
  (when (treesit-ready-p 'hydrangea)
    (treesit-parser-create 'hydrangea)
    (setq-local treesit-font-lock-settings (hydrangea-ts-mode--font-lock-settings))
    (setq-local treesit-font-lock-feature-list hydrangea-ts-mode--font-lock-feature-list)
    (setq-local treesit-simple-indent-rules (hydrangea-ts-mode--indent-rules))
    (setq-local treesit-defun-type-regexp (rx bos "declaration" eos))
    (setq-local treesit-defun-name-function #'hydrangea-ts-mode--defun-name)
    (setq-local treesit-simple-imenu-settings
                '((nil "^declaration$" nil hydrangea-ts-mode--defun-name)))
    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hyd\\'" . hydrangea-ts-mode))

(provide 'hydrangea-ts-mode)

;;; hydrangea-ts-mode.el ends here
