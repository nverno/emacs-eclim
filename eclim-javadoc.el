;;; eclim-javadoc --- major-mode for perusing javadocs -*- lexical-binding: t; -*-
;;
;; This is free and unencumbered software released into the public domain.
;;
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Created: 10 September 2018
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; Formatting for javadocs retrieved from eclim.
;;
;;; Description:
;;
;;; Code:

(eval-when-compile
  (require 'eclim-macros)
  (require 'cl-lib))
(require 'eclim-common)
(require 'button)
(autoload 'shr-render-buffer "shr")

(defgroup eclim-javadoc nil
  "Major mode for perusing javadocs."
  :group 'eclim)

(eclim-bind-keys (:map eclim-javadoc-map :prefix "?" :doc "Eclim Javadoc")
  ("?" . eclim-javadoc-element-at-point)
  ("b" . eclim-javadoc-browse-element-at-point))

(defvar eclim-javadoc-buffer-name "*Javadoc*"
  "Name of buffer to associate with javadocs.")

(defvar eclim-javadoc-sections
  '("Implementation Note" "See Also" "Since"))

(eval-when-compile (defvar eclim-javadoc-history))

;;; buttons
(define-button-type 'eclim-javadoc-xref
  'follow-link t
  'help-echo "mouse-2, RET: display this javadoc"
  'action #'eclim-javadoc-follow-link)

;;; Major mode

(defvar eclim-javadoc-mode-menu
  '("Javadoc"
    ["Previous" eclim-javadoc-previous t]
    ["Render HTML" eclim-javadoc-render-html t]))

(defvar eclim-javadoc-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    (easy-menu-define nil map nil eclim-javadoc-mode-menu)
    (define-key map "p"             #'eclim-javadoc-previous)
    (define-key map "w"             #'eclim-javadoc-render-html)
    (define-key map (kbd "C-c C-z") #'eclim-javadoc-render-html)
    map)
  "Keymap used by `eclim-javadoc-mode'.")

(eval-when-compile
  (defmacro eclim--sym-re (syms &optional follow)
    `(concat "\\_<" (regexp-opt ,syms t) ,(or follow "\\_>"))))

(defvar eclim-javadoc-keywords
  (eval-when-compile
    (let* ((headers '("Since" "Implementation Note" "See Also")))
      `(;; ("\\([[:alnum:] ]+\\)\n" (1 font-lock-doc-face))
        ("&trade;" (0 (ignore (replace-match "™"))))
        (,(eclim--sym-re headers ":") (1 font-lock-constant-face))
        ("[A-Za-z.]+[A-Z][a-z]+" . font-lock-builtin-face)))))

(define-derived-mode eclim-javadoc-mode special-mode "Javadoc"
  "Major mode to examine javadocs output from Eclim."
  (setq-local eclim-javadoc-history nil)
  (setq font-lock-defaults '(eclim-javadoc-keywords nil nil nil)))

(defun eclim-javadoc-buffer ()
  "Return javadoc buffer.
Creates a new one if necessary."
  (or (get-buffer eclim-javadoc-buffer-name)
      (progn
        (set-buffer (get-buffer-create eclim-javadoc-buffer-name))
        (eclim-javadoc-mode)
        (current-buffer))))

(defun eclim--javadoc-current-package (&optional class)
  "Determine the current package for this javadoc.
If CLASS is non-nil, return the path with the CLASS as well."
  (let ((pkg
         (split-string
          (buffer-substring-no-properties
           (point-min)
           (line-end-position  (+ 2 (- (line-number-at-pos))))) "\\."))
        pkg-class)
    (if class
        pkg
      (setq pkg-class (car (last pkg)))
      (if (and pkg-class (string-match-p "^[A-Z]" pkg-class))
          (butlast pkg)
        pkg))))

;; current directory path to documentation (from root)
(defun eclim--javadoc-current-path (&optional class)
  (mapconcat 'identity (eclim--javadoc-current-package class) "/"))

;; fixup lists where items are on different lines
(defun eclim--javadoc-format-lists ()
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (while (re-search-forward "^\t-" nil 'move)
      (while (and (not (eobp)) (looking-at-p "\\s-*$"))
        (delete-char 1))
      (insert " ")
      (end-of-line)
      (delete-horizontal-space)
      (while (and (not (memq (char-before) '(?\; ?\.)))
                  (not (looking-at-p "[\n\t ]*[A-Z-]")))
        (delete-char 1)
        (insert " ")
        (end-of-line)
        (delete-horizontal-space))
      (put-text-property (point) (1+ (point)) 'hard t))))

;; format javadoc buffer: fills long lines, but tries to maintain proper
;; paragraph separation using `use-hard-newlines' and add text properties to the
;; resulting docs
(defun eclim--javadoc-format-buffer ()
  (eclim--javadoc-format-lists)
  (let ((use-hard-newlines t)
        (nlnl "\n\n")
        (nl "\n")
        ;; skip package header and following line
        (pos (progn (goto-char (point-min)) (forward-line 2) (point)))
        in-block)
    (put-text-property 0 1 'hard t nl)
    (put-text-property 0 1 'hard t nlnl)
    (while (re-search-forward
            ;; grab either end of paragraph, start of block (":\n"), or start of
            ;; header "some heading\n" (eg. it doesn't end with a period).
            ;;        1        2               3              4
            "[ \t]*\\(\\([[:alnum:]& ]+\\)?\\([.]\\)? *\\([:\n\t]+\\)\\)" nil 'move)
      (pcase (and (match-string 4) (substring (match-string 4) 0 1))
        (`":"                                       ;start of block or header
         (if (string-match-p "\t" (match-string 4)) ;header
             (if (string-match-p "Since" (match-string 2))
                 (progn
                   (forward-line)       ;Since: 1.0
                   (skip-chars-forward "[ \t0-9.]" (point-at-eol))
                   (insert nl))
               (replace-match (concat ":" nlnl) nil nil nil 4))
           ;; start of code / output block
           (replace-match (concat (match-string 2) ":" nl) nil nil nil 1)
           (setq in-block t)
           (add-text-properties (1- (point)) (point) '(start-block t))))
        (`"\n"
         (cond
          ((and in-block (looking-at "\\(^[^ \t\n]\\)\\|\\s-*$")) ;end of block
           (setq in-block nil)
           (when (not (match-string 1))
             (replace-match nl)
             (forward-line))
           (add-text-properties (1- (point)) (point) '(end-block t)))
          ((match-string 3)                                       ;end of paragraph
           (replace-match nlnl nil nil nil 4))
          ((match-string 2)             ;header
           (replace-match
            (concat nlnl (match-string 2) nl
                    (and (eq (char-after) ?-) nl "\t")) ;start of list
            nil nil nil 1))
          ((memq (char-after (point-at-bol)) '(? ?	))        ;inside block
           (replace-match nl))
          (t                                                      ;header
           (if (match-string 2)
               (replace-match (concat (match-string 2) nlnl))
             (replace-match nlnl)))))
        (`"\t"
         (cond
          ((eq (char-after) ?-) ;in list
           (save-excursion
             (end-of-line)
             (delete-char 1)
             ;;messed up list item
             (and (looking-back "-[ \t]*" (line-beginning-position))
                  (progn (end-of-line)
                         (delete-char 1)))
             (if (eq (char-after) ?\t)
                 (insert nl)            ;list item
               (insert nlnl))))         ;last list item
          ((match-string 2)             ;block
           (replace-match (concat (match-string 2) nl)))
          (t nil)))
        (_ (replace-match nlnl))))
    (fill-region pos (point-max) nil 'nosqueeze)))

;; remove any extraneous stuff from link text
(defsubst eclim--javadoc-clean-link (link-text)
  (replace-regexp-in-string "</?[A-Za-z]+>" "" link-text))

(defun eclim--javadoc-highlight-references (links)
  "Create cross-references for LINKS in javadoc buffer."
  (let (link placeholder text href)
    (dotimes (i (length links))
      (setq link (aref links i)
            text (eclim--javadoc-clean-link (cdr (assoc 'text link)))
            href (cdr (assoc 'href link))
            placeholder (format "|%s[%s]|" text i))
      ;; (insert placeholder href "\n")
      (goto-char (point-min))
      (while (search-forward placeholder nil 'move)
        (replace-match text)
        (make-text-button (match-beginning 0) (match-end 0)
                          'type 'eclim-javadoc-xref
                          'url href)))))

(defun eclim--javadoc-insert-doc-and-format (doc &optional add-to-history)
  "Inserts javadoc DOC into buffer, creates links and formats text.
If ADD-TO-HISTORY is non-nil, the current contents of the buffer are
stored in the buffer-local history stack."
  (let ((inhibit-read-only t))
    (and add-to-history
         (push (buffer-substring (point-min) (point-max))
               eclim-javadoc-history))

    (erase-buffer)
    (insert (cdr (assoc 'text doc)))
    (eclim--javadoc-format-buffer)
    (eclim--javadoc-highlight-references (cdr (assoc 'links doc)))

    (when add-to-history
      (goto-char (point-max))
      (insert "\n\n")
      (insert-text-button "back" 'follow-link t 'action
                          'eclim--javadoc-go-back))
    (goto-char (point-min))))

;; -------------------------------------------------------------------
;;; Navigate links

(defun eclim--javadoc-go-back (_link)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (pop eclim-javadoc-history)))
  (goto-char (point-min)))

(defun eclim-javadoc-previous ()
  "Navigate back to previous page if there is one."
  (interactive)
  (if eclim-javadoc-history
      (eclim--javadoc-go-back nil)
    (message "No previous pages.")))

(defun eclim--javadoc-follow-eclipse-xref (url)
  "Follow the eclipse javadoc URL at point while browsing javadocs."
  (eclim/with-results doc ("java_element_doc" ("-u" url))
    (eclim--javadoc-insert-doc-and-format doc t))
  url)

(defun eclim--javadoc-docpath (path)
  "Return fully qualified PATH to local documentation or nil."
  (or
   (cl-some
    (lambda (var)
      (let ((fullpath (expand-file-name path (symbol-value var))))
        (and (file-exists-p (replace-regexp-in-string "#.+" "" fullpath))
             fullpath)))
    '(eclim-java-documentation-root
      eclim-java-android-documentation-root))
   (prog1 nil
     (message "Can't find the root directory for file: '%s'" path))))

(defun eclim--javadoc-follow-relative-xref (url)
  "Follow URL to a javadoc relative to the current package directory.
Return the full path or nil."
  (let ((path (eclim--javadoc-docpath (eclim--javadoc-current-path))))
    (when path
      (setq path (if (string-prefix-p "#" url)
                     (concat path "/package-summary.html" url)
                   (expand-file-name url path)))
      (and (file-exists-p (replace-regexp-in-string "#.+" "" path))
           (prog1 path
             (browse-url (concat "file://" path)))))))

(defun eclim--javadoc-follow-local-xref (url)
  "Try to follow a local URL to a javadoc.
Return the filepath if found, otherwise nil."
  (let ((path (eclim--javadoc-docpath
               (replace-regexp-in-string "^[./]+" "" url))))
    (prog1 path (browse-url (concat "file://" path)))))

(defun eclim--javadoc-follow-http-xref (url)
  "Follow the external URL.
Replaces encoded characters in link, and returns cleaned url."
  (let ((uri (with-temp-buffer
               (insert url)
               (goto-char (point-min))
               (xml-parse-string))))
    (prog1 uri
      (browse-url uri))))

;; TODO: links starting with '../../../'
(defun eclim-javadoc-follow-link (link)
  "Follow the LINK at point while browsing javadocs.
Return the url or nil if path isn't found."
  (interactive)
  (let ((url (button-get link 'url)))
    (cond
     ((string-prefix-p "eclipse-javadoc" url)
      (eclim--javadoc-follow-eclipse-xref url))
     ((string-prefix-p "\.\." url)
      (eclim--javadoc-follow-local-xref url))
     ((string-prefix-p "http" url)
      (eclim--javadoc-follow-http-xref url))
     (t (eclim--javadoc-follow-relative-xref url)))
    url))

;; -------------------------------------------------------------------
;;; Commands

(defun eclim-javadoc-render-html ()
  "Render the current javadoc as HTML using shr if possible."
  (interactive)
  (let* ((doc (concat (eclim--javadoc-current-path 'class) ".html"))
         (path (eclim--javadoc-docpath doc)))
    (if path
        (with-current-buffer (get-buffer-create (concat "*" doc "*"))
          (insert-file-contents path)
          (shr-render-buffer (current-buffer))
          (pop-to-buffer (current-buffer)))
      (message "File '%s' not found." path))))

;;;###autoload
(defalias 'eclim-java-show-documentation-for-current-element
  'eclim-javadoc-element-at-point)

;;;###autoload
(defun eclim-javadoc-element-at-point ()
  "Displays the doc comments for the element at the point.
Returns the results from eclim or nil."
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if symbol
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (eclim/with-results doc ("java_element_doc"
                                   ("-p" (eclim-project-name))
                                   "-f"
                                   ("-l" (- (cdr bounds) (car bounds)))
                                   ("-o" (save-excursion
                                           (goto-char (car bounds))
                                           (eclim--byte-offset))))

            (pop-to-buffer (eclim-javadoc-buffer))
            (eclim--javadoc-insert-doc-and-format doc)

            ;; return eclim results
            (prog1 doc
              (message (substitute-command-keys
                        (concat
                         "\\[forward-button] - move to next link, "
                         "\\[backward-button] - move to previous link, "
                         "\\[eclim-quit-window] - quit"))))))

      (message "No element found at point.")
      nil)))

;;;###autoload
(defalias 'eclim-java-browse-documentation-at-point
  'eclim-javadoc-browse-element-at-point)
;;;###autoload
(defun eclim-javadoc-browse-element-at-point (&optional arg)
  "Browse the documentation of the element at point.
With the prefix ARG, ask for pattern.  Pattern is a shell glob
pattern, not a regexp.  Rely on `browse-url' to open user defined
browser.  Return the url found or nil."
  (interactive "P")
  (let ((symbol (if arg
                    (read-string "Glob Pattern: ")
                  (symbol-at-point)))
        (proj-name (or (eclim-project-name)
                       (error "Not in Eclim project"))))
    (if symbol
        (let* ((urls (if arg
                         (eclim/execute-command "java_docsearch"
                                                ("-n" proj-name)
                                                "-f"
                                                ("-p" symbol))
                       (let ((bounds (bounds-of-thing-at-point 'symbol)))
                         (eclim/execute-command "java_docsearch"
                                                ("-n" proj-name)
                                                "-f"
                                                ("-l" (- (cdr bounds) (car bounds)))
                                                ("-o" (save-excursion
                                                        (goto-char (car bounds))
                                                        (eclim--byte-offset)))))))
               ;; convert from vector to list
               (urls (append urls nil)))
          (if urls
              (let ((url (if (> (length urls) 1)
                             (eclim--completing-read "Browse: " (append urls nil))
                           (car urls))))
                (prog1 url (browse-url url)))
            (message "No documentation for '%s' found" symbol)
            nil))
      (message "No element at point")
      nil)))

(provide 'eclim-javadoc)
;;; eclim-javadoc.el ends here
