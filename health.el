;;; Unused
(defun health--prompt-table-name ()
  "Collects and prompts a table name"
  (health--prompt-read
   "Table"
   (org-element-map (org-element-parse-buffer 'element) 'table
     (-partial 'org-element-property :name))))

;;; Tables
;;;; Configuration
(setq health-tables-file "c:/~/dev/health/base.org"
      health-workouts-file "c:/~/dev/health/workouts.org")

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
  "Prompts a table and returns its lisp representation"
  (save-excursion
    (find-file health-tables-file)
    (-let* ((tables (health--parse-tables))
            (table-name
             (health--prompt-read "Table" (--map (plist-get it :name) tables)))
            (table (--first (member table-name it) tables))
            ((&plist :contents-begin pos) table))
      (goto-char pos)
      (org-table-to-lisp))))

;;;; Narrow
(defun health--narrow-table (TABLE)
  "Filters TABLE on prompted column"
  (let* ((headers (health--parse-table-headers TABLE))
         (table-data (health--parse-table-data TABLE))
         (filter (health--prompt-read "Filter" headers))
         (column (-elem-index filter headers))
         (options (-distinct (-select-column column table-data))))
    (health--prompt-read "Options" options)))

;;;; Add
(defun health--prompt-from-table ()
  "Prompts a table and appends a prompted row"
  (let* ((table (health--prompt-table))
         (row (health--prompt-row table)))
    (s-join " " row)))

(defun health--add-to-table ()
  "Prompts a table and appends a prompted row"
  (let* ((table (health--prompt-table))
         (row (health--prompt-row table)))
    (-snoc table row)))

;;;; Org Capture
;; Might want to generalize, associate a table with props
(setq health--workout-base-props
      '(("DURATION_ALL" . "5 10 15 20 25 30 35 40 45 50 55 60 75 90")
        ("INTENSITY_ALL" . "easy normal hard"))
      health--workout-cardio-props
      '(("DISTANCE_ALL" . "1 2 3 4 5 6 7 8 9"))
      health--workout-strength-props
      '(())
      health--workout-skill-props
      '(()))

(setq health--cardio-templates
      (let* ((workouts health-workouts-file)
             (hl "Test")

             (base "* DONE %^T Run => %(health--prompt-from-table)")
             (tags ":fitness:cardio:run:")
             (props "%^{DURATION}p %^{INTENSITY}p")
             (end "\n%i%?")

             (full (s-join " " `(,base ,tags ,props ,end))))
        `(("r" "Run" entry (file+headline ,workouts ,hl) ,full))))

(setq org-global-properties health--workout-base-props)
(setq org-capture-templates health--cardio-templates)
