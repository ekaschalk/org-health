;;; Unused
(defun health--prompt-table-name ()
  "Collects and prompts a table name"
  (health--prompt-read
   "Table"
   (org-element-map (org-element-parse-buffer 'element) 'table
     (-partial #'org-element-property :name))))

;;; Tables
;;;; Parsers
(defun health--parse-table-headers (TABLE)
  (car TABLE))
(defun health--parse-table-options (TABLE)
  (--map (s-split "/" it t) (nth 1 TABLE)))
(defun health--parse-table-data (TABLE)
  (-slice TABLE (+ 1 (-elem-index 'hline TABLE))))

(defun health--parse-tables ()
  "Gets list of plists of table properties"
  (org-element-map (org-element-parse-buffer 'element) 'table
    (lambda (table)
      (--when-let (org-element-property :name table)
        (-list :name it :contents-begin
               (org-element-property :contents-begin table))))))

;;;; Prompts
(defun health--prompt-read (HEADER OPTIONS)
  "HEADER prompts OPTIONS a list of strings or nil for non-helm reading"
  (if (not OPTIONS)
      (read-string (concat HEADER ": "))
    (helm :sources (helm-build-sync-source HEADER
                     :candidates OPTIONS
                     :fuzzy-match t))))

(defun health--prompt-row (TABLE)
  "Prompts a row for TABLE according to its headers and options."
  (-zip-with 'health--prompt-read
             (health--parse-table-headers TABLE)
             (health--parse-table-options TABLE)))

(defun health--prompt-table ()
  "Prompts tables and returns its lisp representation"
  (save-excursion
    (-let* ((tables (health--parse-tables))
            (table-name
             (health--prompt-read "Table" (--map (plist-get it :name) tables)))
            (table
             (--first (string= table-name (plist-get it :name)) tables))
            ((&plist :contents-begin pos) table))
      (goto-char pos)
      (org-table-to-lisp))))

;;;; Filters
(defun health--narrow-table (TABLE)
  "Filters TABLE on prompted column"
  (let* ((headers (health--parse-table-headers TABLE))
         (table-data (health--parse-table-data TABLE))
         (filter (health--prompt-read "Filter" headers))
         (column (-elem-index filter headers))
         (options (-distinct (-select-column column table-data))))
    (health--prompt-read "By value" options)))

;;;; Utilities
(defun health--add-to-table ()
  "Prompts and appends row to TABLE"
  (let* ((table (health--prompt-table))
         (row (health--prompt-row table)))
    (-snoc table row)))
