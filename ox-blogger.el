;; muhaha ob das was wired

(require 'ox-html)

(defcustom org-html-blogger-toplevel-hlevel 1
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-html-blogger
  :type 'integer)

(org-export-define-derived-backend 'blogger 'html
  
  :menu-entry
  '(?h 1
       ((?G "As HTML buffer (Blogger)" org-html-blogger-export-as-html)
        (?g "As HTML file (Blogger)" org-html-blogger-export-to-html)
        (?v "As HTML file (Blogger) and open"
            (lambda (a s v b)
              (if a (org-html-blogger-export-to-html t s v b)
                (org-open-file (org-html-blogger-export-to-html nil s v b)))))))

  :option-alist '((:html-toplevel-hlevel nil nil org-html-blogger-toplevel-hlevel))
  :translate-alist '((template . org-html-blogger-template)
                     (headline . org-html-blogger-headline)
                     (table-cell . org-html-blogger-table-cell)
                     (table-row . org-html-blogger-table-row)))

(defun org-html-blogger-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Empty contents?
  (setq contents (or contents ""))
  (let* ((numberedp (org-export-numbered-headline-p headline info))
	 (level (org-export-get-relative-level headline info))
	 (text (org-export-data (org-element-property :title headline) info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 (org-export-get-headline-number
					  headline info) ".")))
	 ;; Create the headline text.
	 (full-text (org-html-format-headline--wrap headline info)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info)
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'ordered 'unordered))
	     (itemized-body (org-html-format-list-item
			     contents type nil info nil full-text)))
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (org-html-begin-plain-list type))
	 itemized-body
	 (and (org-export-last-sibling-p headline info)
	      (org-html-end-plain-list type)))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((section-number (mapconcat 'number-to-string
					(org-export-get-headline-number
					 headline info) "-"))
	     (ids (remove 'nil
			  (list (org-element-property :CUSTOM_ID headline)
				(concat "sec-" section-number)
				(org-element-property :ID headline))))
	     (preferred-id (car ids))
	     (extra-ids (cdr ids))
	     (extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
	     (level1 (+ level (1- org-html-blogger-toplevel-hlevel)))
	     (first-content (car (org-element-contents headline))))
	(format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
		(org-html--container headline info)
		(format "outline-container-%s"
			(or (org-element-property :CUSTOM_ID headline)
			    (concat "sec-" section-number)))
		(concat (format "outline-%d" level1) (and extra-class " ")
			extra-class)
		(format "\n<h%d id=\"%s\">%s%s</h%d>\n"
			level1
			preferred-id
			(mapconcat
			 (lambda (x)
			   (let ((id (org-export-solidify-link-text
				      (if (org-uuidgen-p x) (concat "ID-" x)
					x))))
			     (org-html--anchor id)))
			 extra-ids "")
			full-text
			level1)
		;; When there is no section, pretend there is an empty
		;; one to get the correct <div class="outline- ...>
		;; which is needed by `org-info.js'.
		(if (not (eq (org-element-type first-content) 'section))
		    (concat (org-html-section first-content "" info)
			    contents)
		  contents)
		(org-html--container headline info)))))))

(defun org-html-blogger-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
      (concat "<html"
	   (when (org-html-xhtml-p info)
	     (format
	      " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
	      (plist-get info :language) (plist-get info :language)))
	   ">\n")
      (org-html-blogger-style-sheet)
      "<body>\n"
   ;; Document contents.
   (format "<%s id=\"%s\">\n"
	   (nth 1 (assq 'content org-html-divs))
	   (nth 2 (assq 'content org-html-divs)))

   ;; Document title.
    contents
   (format "</%s>\n"
	   (nth 1 (assq 'content org-html-divs)))
   ;; Closing document.
   "</body>"))



(defun org-html-blogger-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
	 (table (org-export-get-parent-table table-cell))
	 (cell-attrs
	  (if (not org-html-table-align-individual-fields) ""
	    (format (if (and (boundp 'org-html-format-table-no-css)
                       org-html-format-table-no-css)
                  " align=\"%s\"" " class=\"%s\"")
              (org-export-table-cell-alignment table-cell info)))))
    
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents "&#xa0;"))
    (cond
     ((and (org-export-table-has-header-p table info)
	   (= 1 (org-export-table-row-group table-row info)))
      (concat "\n" (format (car org-html-table-header-tags) "col" (replace-regexp-in-string "\"$" " mytablehead\"" cell-attrs))
         
	      contents (cdr org-html-table-header-tags)))
     ((and org-html-table-use-header-tags-for-first-column
	   (zerop (cdr (org-export-table-cell-address table-cell info))))
      (concat "\n" (format (car org-html-table-header-tags) "row" (replace-regexp-in-string "\"$" " mytablerow\"" cell-attrs))
	      contents (cdr org-html-table-header-tags)))
     (t (concat "\n" (format (car org-html-table-data-tags) (replace-regexp-in-string "\"$" " mytablecell\"" cell-attrs))
		contents (cdr org-html-table-data-tags))))))

;;;; Table Row

(defun org-html-blogger-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((rowgroup-number (org-export-table-row-group table-row info))
	   (row-number (org-export-table-row-number table-row info))
	   (start-rowgroup-p
	    (org-export-table-row-starts-rowgroup-p table-row info))
	   (end-rowgroup-p
	    (org-export-table-row-ends-rowgroup-p table-row info))
	   ;; `top-row-p' and `end-rowgroup-p' are not used directly
	   ;; but should be set so that `org-html-table-row-tags' can
	   ;; use them (see the docstring of this variable.)
	   (top-row-p (and (equal start-rowgroup-p '(top))
			   (equal end-rowgroup-p '(below top))))
	   (bottom-row-p (and (equal start-rowgroup-p '(above))
			      (equal end-rowgroup-p '(bottom above))))
	   (rowgroup-tags
	    (cond
	     ;; Case 1: Row belongs to second or subsequent rowgroups.
	     ((not (= 1 rowgroup-number))
	      '("<tbody>" . "\n</tbody>"))
	     ;; Case 2: Row is from first rowgroup.  Table has >=1 rowgroups.
	     ((org-export-table-has-header-p
	       (org-export-get-parent-table table-row) info)
	      '("<thead>" . "\n</thead>"))
	     ;; Case 2: Row is from first and only row group.
	     (t '("<tbody>" . "\n</tbody>")))))
      (concat
       ;; Begin a rowgroup?
       (when start-rowgroup-p (car rowgroup-tags))
       ;; Actual table row
       (concat "\n" (eval (car org-html-table-row-tags))
	       contents
	       "\n"
	       (eval (cdr org-html-table-row-tags)))
       ;; End a rowgroup?
       (when end-rowgroup-p (cdr rowgroup-tags))))))


;;;###autoload
(defun org-html-blogger-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'blogger "*Org HTML (Blogger) Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-html-blogger-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'blogger file
      async subtreep visible-only body-only ext-plist)))

(defun org-html-blogger-style-sheet ()
  "Insert custom inline css"
  (concat
   "<style>\n"
   (with-temp-buffer
     (insert-file-contents "/home/eichberger/emacsimize/google-blogger-api/style.css")
     (buffer-string))
   "</style>\n"))

(provide 'ox-blogger)
