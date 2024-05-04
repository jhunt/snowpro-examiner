(defvar *facts* nil)
(setf *random-state* (make-random-state t))

(defun flatten (l)
  (cond ((null l) nil)
        ((null (car l))
         (flatten (cdr l)))
        ((atom (car l))
         (cons (car l)
               (flatten (cdr l))))
        (t (append (flatten (car l))
                   (flatten (cdr l))))))

(defun shuffle (l)
  (loop for i from (length l) downto 2
        do (rotatef (elt l (random i))
                    (elt l (1- i))))
  l)

(defun riffle (a b)
  (shuffle (append a b)))

(defun stacked-riffle (a b)
  (let ((aa (shuffle a)))
    (cons (car aa)
          (riffle (cdr aa) b))))

(defun remove-nth (n l)
  (if (or (zerop n) (null l))
      (cdr l)
      (cons (car l) (remove-nth (1- n) (cdr l)))))

(defun pick-n-from (n from)
  (cond ((or (eq n 0)
             (null from)) nil)
        (t
         (let ((i (random (length from))))
           (cons (elt from i)
                 (pick-n-from (1- n)
                              (remove-nth i from)))))))

(defun coin-flip ()
  (car (shuffle (list t nil))))

(defun prefix (a l)
  (cond ((null l) nil)
        (t (cons (cons a (car l))
                 (prefix a (cdr l))))))

(defun repeat (l v)
  (cond ((null l) nil)
        (t (cons v (repeat (cdr l) v)))))

(let ((id 0))
  (defun next-id ()
    (incf id)))

(defclass question ()
  ((number :initform (next-id))
   (name   :initarg :name)))

(defclass yes-no-question (question)
  ((true-assertion  :initarg :true)
   (false-assertion :initarg :false)))

(defclass multiple-choice-question (question)
  ((question  :initarg :question)
   (n-choices :initarg :n-choices)
   (correct   :initarg :correct)
   (incorrect :initarg :incorrect)))

(defclass prepared-question (question)
  ((text :initarg :text)
   (allowed-answers :initarg :allowed)
   (correct-answers :initarg :correct)))

(defun remove-item (e l)
  (cond ((null l) nil)
        ((eq e (car l)) (remove-item e (cdr l)))
        (t (cons (car l)
                 (remove-item e (cdr l))))))

(defun set-equal (a b)
  (cond ((and (null a) (null b)) t)
        ((or  (null a) (null b)) nil)
        (t (set-equal (cdr a)
                      (remove-item (car a) b)))))

(defmethod answered-correctly? ((q prepared-question) answers)
  (cond ((eq 'y answers)
         (set-equal (list 't)
                    (slot-value q 'correct-answers)))
        ((eq 'n answers)
         (set-equal (list 'f)
                    (slot-value q 'correct-answers)))
        ((atom answers)
         (set-equal (list answers)
                    (slot-value q 'correct-answers)))
        (t
         (set-equal answers
                    (slot-value q 'correct-answers)))))

(defmethod frame-question ((q yes-no-question))
  (cond ((coin-flip)
         (make-instance 'prepared-question
            :text (format nil "~A~%~%T or F? "
                          (slot-value q 'true-assertion))
            :allowed '(T F)
            :correct '(T)))
        (t
         (make-instance 'prepared-question
            :text (format nil "~A~%~%T or F? "
                          (slot-value q 'false-assertion))
            :allowed '(T F)
            :correct '(F)))))

(defmethod frame-question ((q multiple-choice-question))
  (let* ((answers
           (shuffle
             (subseq (stacked-riffle
                       (prefix t   (slot-value q 'correct))
                       (prefix nil (slot-value q 'incorrect)))
                     0 (slot-value q 'n-choices))))
         (prompts nil)
         (allowed nil)
         (correct nil))
    (loop for answer in answers
          for letter in '(a b c d e f g h)
          do
          (and (car answer) (push letter correct))
          (push (format nil "~A) ~A" letter (cdr answer))
                prompts)
          (push letter allowed))
    (make-instance 'prepared-question
      :text (format nil "~A~%~%~{  ~A~%~}~%Your choices: "
                    (slot-value q 'question)
                    (reverse prompts))
      :allowed allowed
      :correct (reverse correct))))

(defun clear ()
  (setq *facts* nil))

(defun prep-exam ()
  (mapcar #'frame-question
          (shuffle (copy-list *facts*))))

(defun game-until (maxq)
  (let* ((score 0)
         (keep-going t)
         (exam (prep-exam)))

    (loop while keep-going
          for i from 0 to (- maxq 1)
          for q in exam
          do
          (format t "~%~A) ~A" (1+ i) (slot-value q 'text))
          (let ((answer (read)))
            (cond ((eq answer 'q)
                   (format t "Exiting...")
                   (setf keep-going nil))
                  ((answered-correctly? q answer)
                   (incf score))
                  (t
                   (format t "Incorrect!~%")
                   (format t "Correct answer was ~S~%"
                           (slot-value q 'correct-answers))
                   (setf keep-going nil)))))
    (format t "Final Score: ~A~%" score)
    (if keep-going
        (format t "Congratulations on your PERFECT SCORE!~%"))
    (format t "~%Go again? (Y/N) ")
    (if (eq 'y (read))
        (game-until maxq))))

(defun banner ()
  (format t "~%~%")
  (format t "     \\__  __/~%")
  (format t "     /_/  \\_\\        Snowflake SnowPro Quiz ❄️   ~%")
  (format t "      _\\/\\/_         ~%")
  (format t " __/\\_\\_\\/_/_/\\__    Think you know Snowflake?~%")
  (format t "   \\/ /_/\\_\\ \\/      ~%")
  (format t "     __/\\/\\__        Answer T/F questions by typing T or F.~%")
  (format t "     \\_\\  /_/        Answer multiple choice with single letter~%")
  (format t "     /      \\        choices wrapped in parentheses, i.e. (A C D)~%")
  (format t "~%")
  (format t "                     made with ❤️ by vivanti.~%")
  (format t "~%")
  (format t "~%"))

(defun run ()
  (banner)
  (format t "~A facts known to the examiner~%" (length *facts*))
  (game-until (length *facts*)))

(defun fact (subj verbs obj)
  (push
    (make-instance 'yes-no-question
                   :true (format nil "~{~A ~}"
                                 (flatten
                                   (list subj (car verbs) obj)))
                   :false (format nil "~{~A ~}"
                                  (flatten
                                    (list subj (cadr verbs) obj))))
    *facts*))

(defun multi (text n right wrong)
  (push
    (make-instance 'multiple-choice-question
      :question text
      :n-choices n
      :correct right
      :incorrect wrong)
    *facts*))

(let ((are   '("are" "are not"))
      (arent '("are not" "are"))
      (isnt  '("is not" "is"))
      (will  '("will" "will not"))
      (do    '(nil "do not"))
      (dont  '("do not" nil))
      (can   '("can" "cannot"))
      (cant  '("cannot" "can")))
  (fact "Materialized views" are "a type of table")
  (fact "Dynamic tables" are "a type of table")
  (fact "Transient tables" are "a type of table")
  (fact "Temporary tables" are "a type of table")

  (fact "Transient tables" do "persist beyond the session that defined them")
  (fact "Transient tables" are "visible to other roles pursuant to granted privileges")
  (fact "Transient tables" arent "protected by failsafe")
  (fact "Transient tables" are "protected by time travel")
  (fact "A transient table" can  "be cloned as a temporary table")
  (fact "A transient table" cant "be cloned as a permanent table")
  (fact "A permanent table" can  "be cloned as a transient table")
  (fact "You" can "undrop a transient table")

  ;; temporary tables
  (fact "Temporary tables" dont "persist beyond the session that defined them")

  (fact "Temporary tables" arent "visible to other roles pursuant to granted privileges")
  (fact "Temporary tables" can "have the same names as non-temporary tables in the same database and schema")
  (fact "Temporary tables" arent "protected by failsafe")
  (fact "Temporary tables" are   "protected by time travel")

  (fact "A temporary table" can  "be cloned as a transient table")
  (fact "A temporary table" cant "be cloned as a permanent table")
  (fact "A permanent table" can  "be cloned as a temporary table")
  (fact "You" can "undrop a temporary table")

  ;; permanent tables
  (fact "Permanent tables" are "protected by failsafe")
  (fact "Permanent tables" are "protected by time travel")
  (fact "Permanent tables have"
        '("only 1 day" "up to 90 days")
        "of time travel protection in standard editions of snowflake")

  (fact "Permanent tables have"
        '("up to 90 days" "only 1 day")
        "days of time travel protection in Enterprise editions of snowflake")

  (fact "Materialized views" are "available in Enterprise editions of snowflake and above")
  (fact "Materialized views" arent "available in standard editions of snowflake")
  (fact "If the time travel retention period for a table is set to 0 it"
        will "immediately enter the failsafe period when dropped")
  (fact "A long-running time travel query"
        will "delay the purging of temporary and transient tables until the query completes")
  (fact "The failsafe period" isnt "configurable for tables in Standard edition")
  (fact "The failsafe period" isnt "configurable for tables in Enterprise edition")

  ;; clones; perm -> perm/te/tr; te/tr -> tr/te

  (fact "You" can "clone a temporary table and make transient table out of it")
  (fact "When an object is cloned,"
        '("only the privileges on contained objects (but not those of the object itself)"
          "the object privileges and all contained object privileges")
        "are copied")
  (fact "Grants on a cloned object"
        '("can be copied via the COPY GRANTS subclause."
          "cannot be copied and must be replicated manually.")
        nil)
  (fact "You" can "clone a temporary table and make transient table out of it")

  (fact "Views" '("consume no" "consume") "storage")
  (fact "Views" do "require compute to generate their results")
  (fact "Materialized views" '("consume" "do not consume") "storage")
  (fact "Materialized views" are "fast to retrieve results because they are not computed at query time")
  (fact "Materialized views" cant "lag behind their source tables")
  (fact "Materialized views" do "incur compute to keep them up-to-date")
  (fact "Dynamic tables" '("consume" "consume no") "storage")
  (fact "Dynamic tables" do "require compute to keep them up-to-date")
  (fact "Dynamic tables" are "particularly fast to return results")
  (fact "Dynamic tables" do "offer fine-grained control over when data is refreshed")

  (fact "In a managed schema, only the"
        '("schema" "object")
        "owner can grant privileges on contained objects")

  (fact "In an unmanaged schema, only the"
        '("object" "schema")
        "owner can grant privileged on contained objects")

  ;; streams
  (fact "Append-only streams" can "be created for standard tables")
  (fact "Append-only streams" cant "be created for external and directory tables")
  (fact "Snowflake automatically"
        '("extends" "does not extend")
        "the data retention period for streams on tables with less than 14 days of time travel")

  (fact "When snowflake extends the data retention period to accommodate a stream, the underlying table" isnt "extended")

  (fact "The stale_after column in the output of a show streams query"
        '("shows" "does not show")
        "when the stream must be consumed to avoid problems")

  (fact "Recreating the table or view underneath a stream"
        '("requires" "does not require")
        "the stream to be recreated")

  (fact "Streams track the immutable rowid of changed rows in the"
        '("meta$rowid" "__identity")
        "column of the stream")

  (fact "Streams track the DML operation recorded in the"
        '("meta$action" "meta$dml")
        "column of the stream")

  (fact "Streams track whether or not the change record participated in an update dml operation via the"
        '("meta$isupdate" "meta$updated")
        "column of the stream")

  (fact "You" can "create a stream on top of a shared view.")

  (fact "Streams support"
        '("both local and shared"
          "only local")
        "views as underlying source objects.")

  (fact "Streams support views"
        '("only if the underlying tables have change tracking enabled."
          "even if the underlying tables do not have change tracking enabled.")
        nil)

  (fact "The CHANGES clause of the SELECT query"
        isnt "a viable long-term strategy for change tracking.")

  (fact "Snowflake"
        '("ensures that only one instance of a scheduled task runs at once."
          "allows scheduled task runs to 'lap' one another to avoid skipping scheduled runs.")
        nil)

  (fact "Daylight savings time"
        '("may cause unpredictability when tasks are scheduled in time zones that observe it."
          "does not cause issues when scheduling tasks, as Snowflake converts all schedules to UTC.")
        nil)

  (fact "All tasks in the same task graph"
        '("must have the same owner, and exist in the same database and schema."
          "may have different owners, and reside in different database and/or schemas.")
        nil)
  (fact "Finalizer tasks run"
        '("even if any of the tasks in the task graph fail."
          "only after all tasks in the task graph complete successfully.")
        nil)

  (multi "Which of the following are types of tables in snowflake?"
         3
         '("Permanent Tables"
           "Dynamic Tables"
           "Iceberg Tables"
           "External Tables")
         '("Ephemeral Tables"
           "Static Tables"
           "Super Tables"))

)

  (multi "Which types of objects can be shared between Snowflake Accounts?"
         4
         (list "Databases"
               "Tables"
               "Dynamic Tables"
               "External Tables"
               "Iceberg Tables"
               "Secure Views"
               "Secure Materialized Views"
               "Secure UDFs")
         (list "Users"
               "Roles"
               "Views"
               "Materialized Views"
               "Temporary Tables"))

  (fact "Database objects shared between accounts are"
        '("read-only; data in them cannot be modified or added to"
          "read-write; data in them can be modified.")
        nil)

  (fact "Shared data"
        '("does not take up" "takes up")
        "space in the in the consumer's account.")

  (multi "Which of the following are types of shares?"
         3
         (list "Direct Share"
               "Listing"
               "Data Exchange")
         (list "Private Share"
               "Unlimited Share"))

  (fact "You"
        '("can" "cannot")
        "convert a Direct Share into a Listing after the fact.")
  (fact "You"
        '("cannot" "can")
        "convert a Data Exchange into a Listing after the fact.")
  (fact "You can create"
        '("only one database" "multiple databases")
        "per Share")

  (fact "You"
        '("can share data with a non-Snowflake entity via Reader Accounts."
          "cannot share data in Snowflake with entities without a Snowflake account.")
        nil)
  (fact "A reader account"
        '("can only query data in the share(s) provided to it, and cannot load data, create users, issue grants, etc."
          "can do all of the normal DML operations, including data loading and temporary table creation")
        nil)

  (multi "Listings in the Snowflake marketplace can be made available:"
         3
         (list "Privately"
               "Publicly")
         (list "Managed"
               "Conditionally"))

  (multi "Which of the following are pricing options for Snowflake marketplace listings?"
         3
         (list "Free"
               "Trial"
               "Paid")
         (list "Subscription"
               "Barter"))

   (fact "A database role"
         '("exists within a single database"
           "exists at the account level but is constrained to privileges on database objects")
         nil)
   (fact "The OWNERSHIP privilege on contained objects in the database"
         '("can" "cannot")
         "be granted to a database role.")

   (fact "Without database roles,"
         '("administrators can only grant full privileges to objects in a consumed share."
           "administrators can still grant fine-grained permissions on share contents via normal RBAC.")
         nil)
   (fact "Holders of the MANAGE GRANTS privilege"
         '("can grant new privileges to their current role"
           "can only grant new privileges to other roles")
         nil)

   (multi "Which of the following SQL queries will share database D1 via share S1?"
          3
          (list "grant usage on database D1 to share S1;")
          (list "share database D1 via share S1;"
                "alter share S1 add database D1;"
                "call system$share('D1', 'S1');"))

   (multi "Which of the following SQL queries will allow account org1.c1 to consume the share S1?"
          3
          (list "alter share S1 add accounts = org1.c1;")
          (list "alter share S1 add account org1.c1;"
                "extend share S1 to account org1.c1;"
                "call system$share('S1', 'C1');"))

    (fact "Renaming a database role to move it to another database"
          '("is not allowed."
            "is allowed, but causes the downstream consumers to lose access to the role.")
          nil)

    (fact "Privileges"
          '("can" "cannot")
          "be granted directly to a share.")

    (fact "To share data from multiple databases,"
          '("the share"
            "the involved secure views")
          "must be granted the REFERENCE_USAGE privilege.")

    (fact "By default, Snowflake"
          '("does not allow"
            "allows")
          "sharing from Business Critical accounts to Enterprise or lower accounts.")

    (multi "What privilege must be granted to allow a user on a Business Critical account to share data with an Enterprise or lower account?"
           3
           (list "OVERRIDE SHARE RESTRICTION")
           (list "IGNORE SHARE PROTECTION"
                 "SHARE ADMIN"
                 "CREATE SHARE"))

    (fact "Snowflake support is"
          '(nil "not")
          "required to set up a Data Exchange")

    (multi "Which of the following pieces of information are necessary for setting up a Data Exchange"
           4
           (list "a description of the Business Case"
                 "a (unique) SQL name"
                 "a display name"
                 "the hosting organizations ID"
                 "the hosting account URL")
           (list "expected usage / volume"
                 "account identifiers for all participants"
                 "an email address"))

    (fact "Direct shares are"
          '("not permitted to cross region boundaries"
            "can cross region boundaries and share with Snowflake customers in other clouds")
          nil)

    (fact "Marketplace Listings"
          '("can cross region boundaries and share with Snowflake customers in other clouds"
            "are not permitted to cross region boundaries")
          nil)

    (fact "Micropartitions"
          '("are immutable and cannot be modified once created."
            "can be modified as needed, after creation.")
          nil)

    (multi "What modification clause should be added to a COPY INTO <location> query to cause only one file to be created?"
           3
           (list "SINGLE = TRUE")
           (list "OVERWRITE"
                 "MAX_FILE_SIZE = 0"
                 "NO_SPLIT = TRUE"))

    (fact "@~st1 refers to a"
          '("user stage, for a user name ST1"
            "table stage, for the ST1 table")
          nil)
    (fact "@%st1 refers to a"
          '("table stage, for the ST1 table"
            "user stage, for a user name ST1")
          nil)

    (multi "Which of the following are considered semi-structured file formats to Snowflake?"
           4
           (list "JSON"
                 "Avro"
                 "Parquet"
                 "ORC"
                 "XML")
           (list "CSV"
                 "PDF"))

   (multi "Which of the following are valid scaling policies for a multi-cluster warehouse?"
          4
          (list "Standard"
                "Economy")
          (list "Maximized"
                "Minimized"
                "Conservative"))

  (fact "Network policies currently support"
        '("only IPv4 addresses"
          "both IPv4 and IPv6 addresses")
        nil)

;; size of snowpipe data
;; limits and maxes
;; json structure navigation

   (fact "The VALIDATE function"
         '("returns errors for a single failed COPY INTO execution, by job id"
           "returns errors from all previous runs of failed COPY INTO executions")
         nil)

   (multi "How often does period rekeying happen?"
          4
          (list "every 12 months")
          (list "every month"
                "every 2 years"
                "daily"))

   (fact "Secure data sharing"
         '("cannot" "can")
         "be done in Snowflake VPS environments")

   (fact "Filtering of input data should be done"
         '("as early as possible"
           "as often as possible")
         nil)

(multi "Which of the following are layers of the Snowflake architecture?"
       3
       (list "Database Storage"
             "Query Processing"
             "Cloud Services")
       (list "ETL"
             "Security & Monitoring"
             "Network Routing"))

(fact "Warehouses are billed on a"
      '("per-second runtime basis with a minimum of 60s runtime"
        "per-minute runtime basis with a minimum of 1m runtime")
      nil)

(multi "Serverless credits are consumed for which of the following types of work?"
       4
       (list "Automatic clustering"
             "COPY INTO operations"
             "Data Quality Metric Functions"
             "External Table Refresh"
             "Hybrid Table Operation"
             "Materialized View Maintenance"
             "Logging & Monitoring"
             "Replication"
             "Snowpipe / Streaming"
             "Task Execution")
       (list "Metadata queries"
             "DROP / CREATE DDL queries"
             "Query Cache Maintenance"))

(fact "Cloud services charges"
      '("begin to accumulate after cloud services exceed 10% of daily warehouse usage"
        "are assessed any time cloud services are used")
      nil)

(multi "Cloud Services (in Snowflake) are used to fulfil which of the following?"
       3
       (list "Authentication Services"
             "Infrastructure Management"
             "Metadata Management"
             "Query Parsing"
             "Query Optimization"
             "Access Control Services")
       (list "Query Execution"
             "Materialized View Maintenance"
             "Software Updates"
             "Stage File Storage"))

(fact "Views in the ACCOUNT_USAGE schema"
      '("have 1-3h of latency due to metadata processing time"
        "are realtime")
      nil)

(fact "The Search Optimization Service"
      '("can help with equality and IN (membership) predicates"
        "can only optimize equality predicates")
      nil)

(multi "Which of the following are Snowflake Connectors?"
       5
       (list "Snowflake for Python"
             "Snowflake for Kafka"
             "Snowflake for Spark")
       (list "Snowflake ODBC"
             "Snowflake for Go"
             "Snowflake for PHP"))

(let* ((can '("can" "cannot"))
       (cannot (reverse can)))
  ;; Replication
  (loop for what in
        '("Permanent Tables"
          "Transient Tables"
          "Databases"
          "Schemas"
          "Shares"
          "Views"
          "Materialized Views") do
        (fact what can "be replicated."))
  (loop for what in
        '("Temporary Tables"
          "External Tables"
          "Users"
          "Roles"
          "Integrations"
          "Stages"
          "Tasks"
          "Pipes") do
        (fact what cannot "be replicated."))

  ;; Shares
  (loop for what in
        '("External Tables"
          "Tables"
          "Secure Views"
          "Secure Materialized Views"
          "Secure UDFs") do
        (fact what can "be shared."))
  (loop for what in
        '("Roles"
          "Users"
          "Stages"
          "Temporary Tables") do
        (fact what cannot "be shared."))
)

(fact "Standard edition allows for replication of"
      '("only database and share objects"
        "all objects")
      nil)

(fact "Transient and temporary tables have a maximum retention period of"
      '("1 day" "90 days")
      "in Enterprise edition.")

;; search optimization service
;;
(fact "The Kafka connector"
      '("subscribes to one or more Kafka topics."
        "can only subscribe to a single topic.")
      nil)
(fact "The Kafka connector"
      '("creates new tables for unmapped topics, transparently."
        "ignores topics that are not explicitly mapped to a Snowflake table.")
      nil)
(fact "The Kafka connector"
      '("supports explicit mapping of topics to existing Snowflake tables."
        "only supports landing data into tables it creates and maps to topics.")
      nil)
(fact "Reducing data retention time on a table"
      '("immediately moves data in Time Travel that is outside of the new retention period into failsafe"
        "has no effect on data in Time Travel that is outside of the new retention period")
      nil)

(fact "Increasing data retention time on a table"
      '("causes data in Time Travel to remain there, and honor the new (longer) retention period"
        "has no effect on data in Time Travel; it will continue to honor the old (shorter) retention period")
      nil)


(multi "Which of the following are Cloud Partner Categories?"
       4
       (list "Data Integration"
             "Business Intelligence (BI)"
             "Machine Learning & Data Science"
             "Security Governance & Observability"
             "SQL Development & Management"
             "Native Programmatic Interfaces")
       (list "Application Development"
             "Professional Services"
             "Infrastructure & Platforms"
             "Spark"))

(fact "Snowflake retains batch load history for"
      '("64 days" "14 days")
      nil)


(let* ((supports '("supports" "does not support"))
       (doesnt-support (reverse supports)))
  (loop for what in
        '("DATE, TIME, and TIMESTAMP"
          "BINARY"
          "VARCHAR"
          "Fixed-point (INTEGER, NUMERIC)") do
        (fact "The Search Optimization Service" supports (list what "columns")))
  (loop for what in
        '("Semi-structured"
          "Floating-point") do
        (fact "The Search Optimization Service" doesnt-support (list what "columns"))))

(fact "A TASK can"
      '("only call a single statement."
        "call multiple statements via an implicit BEGIN block.")
      nil)

(fact "A STORED PROCEDURE can"
      '("run as either the caller or the owner"
        "only run as the caller")
      nil)
(fact "A user-defined FUNCTION can"
      '("only run as the caller"
        "run as either the caller or the owner")
      nil)


;; file urls and unstructured
;; INSERT .. OVERWRITE?
;; FLATTEN OUTER->TRUE (make a row for zero-row expansions)
;; how do you remove files from an internal stage?
;;

(multi "Which of the following authentication methods are supported for REST API access?"
       3
       (list "RSA Key Pair"
             "OAuth Flow")
       (list "Username and Password"))

(multi "Which of the following compression methods are supported by Snowflake?"
       4
       (list "raw_deflate"
             "brotli"
             "bzip2"
             "gzip"
             "deflate"
             "zstandard")
       (list))

(fact "Snowpipe loads"
      '("use serverless compute"
        "use a user-specified warehouse")
      "to load data.")

(fact "A scoped URL can"
      '("only be accessed by the user who generated it"
        "can be shared with other users for access")
      nil)
(fact "A file URL can"
      '("can be shared with other users for access"
        "only be accessed by the user who generated it")
      nil)

(fact "A scoped URL"
      '("expires when the persisted query result period ends (24h)"
        "never expires")
      nil)

(fact "A scoped URL"
      '("provides access without needing underlying STAGE privileges."
        "still requires the accessor to have rights to the underlying STAGE.")
      nil)

(fact "A pre-signed URL"
      '("provides access without needing ANY privileges or external authentication"
        "still requires the accessor to have rights to the underlying STAGE.")
      nil)
(fact "A pre-signed URL"
      '("has a custom expiration time set (on a per-URL basis)"
        "expires when the query results cache expires (24h)")
      nil)

(fact "The SQL API"
      '("can" "cannot")
      "be used to call stored procedures")

(fact "An XS Warehouse has"
      '(8 16)
      "threads of execution")

(multi "A high number of which values in SYSTEM$CLUSTERING_INFORMATION indicates that the table is not well clustered?"
      4
      '("average_overlaps"
      "average_depth")
      '("total_partition_count"
      "total_constant_partition_count")
)


(multi "For which types of warehouses MUST the maximum and minimum number of clusters be equal?"
      4
      '("single-cluster warehouses"
      "warehoues in maximized mode")
      '("multi-cluster warehouses"
      "warehoues in auto-scale mode")
)

(fact "An external function's identifier"
      '("doesn’t have to be unique because functions are identified and resolved by their name and argument types" "has to be unique")
      "")


(fact "If a multi-cluster warehouse is resized, the new size"
      '("applies" "does not apply")
      "to all the clusters for the warehouse, including clusters that are currently running and any clusters that are started after the multi-cluster warehouse is resized.")


(fact "When a database is dropped, the data retention period for child schemas or tables, if explicitly set to be different from the retention of the database,"
      '("is not" "is")
      "honored.")

(fact "You"
      '("cannot" "can")
      "access data held in archival cloud storage classes that require restoration.")

(fact "The Search Optimization maintenance service"
      '("automatically updates" "does not automatically update")
      "the search access path to reflect the changes to the data of a table.")

(multi "Which column specifies the timestamp in which the stream will become stale if not consumed?"
      3
      '("stale_after")
      '("stale"
      "mode"
      "created_on")
)

(multi "To use Snowflake-managed compute resources for a task"
      3
      '("omit the WAREHOUSE parameter")
      '("set WAREHOUSE = 'serverless'"
      "contact Snowflake support"
      )
)

(multi "Assuming that the session parameter USE_CACHED_RESULT is set to false, which action will start a virtual warehouse in Snowpark?"
      4
      '("Calling a Snowpark stored procedure to query the database with session.call().")
      '("Creating a DataFrame from a table."
      "Creating a DataFrame from a staged file with the read() method."
      "Transforming a DataFrame with methods like replace()."
      )
)


(multi "How does LATERAL FLATTEN differ from FLATTEN?"
      3
      '("LATERAL FLATTEN joins the FLATTEN output with information outside the object.")
      '("They are the same."
      "LATERAL FLATTEN joins the FLATTEN output with information inside the object."
      )
)

   (fact "You"
         '("can" "cannot")
         "add a clustering key to an existing table or change the existing clustering key for a table using ALTER TABLE.")

(multi "How does Snowflake recommend splitting files?"
      3
      '("By line")
      '("By column"
      "By megabytes"
      )
)

(multi "What is the function of SnowCD?"
      3
      '("It helps users to diagnose and troubleshoot their network connection to Snowflake.")
      '("It allows users to connect to Snowflake using the CMD."
      "It’s an extension of SnowSQL that allow ACCOUNTADMINS to execute admin commands from the CMD."
      )
)


(multi "The DATA_SHARING_USAGE schema in the SNOWFLAKE database is available to:"
      4
      '("The ACCOUNTADMIN role"
      "Any role that has priviledges on the schema")

      '("The SYSADMIN role"
      "The ORGADMIN role"
      "Any role that has priviledges on the database"
      )
)

(multi "The DATA_SHARING_USAGE schema in the SNOWFLAKE database is available to:"
      4
      '("The ACCOUNTADMIN role"
      "Any role that has priviledges on the schema")
      
      '("The SYSADMIN role"
      "The ORGADMIN role"
      )
)

(multi "A Data Engineer is building a set of reporting tables to analyze consumer requests by region for each of the Data Exchange offerings annually, as well as click-through rates for each listing.
Which views are needed MINIMALLY as data sources?"
      4
      '("SNOWFLAKE.DATA_SHARING_USAGE.LISTING_CONSUMPTION_DAILY"
      "SNOWFLAKE.DATA_SHARING_USAGE.LISTING_TELEMETRY_DAILY")
      
      '("SNOWFLAKE.DATA_SHARING_USAGE.LISTING_EVENTS_DAILY"
      "SNOWFLAKE.ACCOUNT_USAGE.DATA_TRANSFER_HISTORY"
      )
)

(multi "Which function will compute a 'fingerprint' over an entire table, query result, or window to quickly detect changes to table contents or query results?"
      4
      '("HASH_AGG(*)"
        "HASH_AGG(<expr>, <expr>)")
      
      '("HASH(*)"
      "HASH_COMPARE(*)"
      )
)

(multi "Which stages support external tables?"
      4
      '("External stages only; from any region, and any cloud provider")
      
      '("Internal stages only; within a single Snowflake account"
      "Internal stages only; from any Snowflake account in the organization"
      "External stages only; only on the same region and cloud provider as the Snowflake account"
      )
)


(multi "Company A and Company B both have Snowflake accounts. Company A's account is hosted on a different cloud provider and region than Company B's account. Companies A and B are not in the same Snowflake organization.
How can Company A share data with Company B? (Choose two.)"
      4
      '("Create a share within Company A's account and add Company B's account as a recipient of that share."
      "Create a new account within Company A's organization in the same cloud provider and region as Company B's account. Use database replication to replicate Company A's data to the new account. Create a share within the new account, and add Company B's account as a recipient of that share.")
      
      '("Create a share within Company A's account, and create a reader account that is a recipient of the share. Grant Company B access to the reader account."
      "Use database replication to replicate Company A's data into Company B's account. Create a share within Company B's account and grant users within Company B's account access to the share."
      "Create a separate database within Company A's account to contain only those data sets they wish to share with Company B. Create a share within Company A's account and add all the objects within this separate database to the share. Add Company B's account as a recipient of the share."
      )
)

(multi "A Data Engineer is working on a continuous data pipeline which receives data from Amazon Kinesis Firehose and loads the data into a staging table which will later be used in the data transformation process. The average file size is 300-500 MB.
The Engineer needs to ensure that Snowpipe is performant while minimizing costs.
How can this be achieved?"
      4
      '("Decrease the buffer size to trigger delivery of files sized between 100 to 250 MB in Kinesis Firehose.")
      
      '("Change the file compression size and increase the frequency of the Snowpipe loads."
      "Split the files before loading them and set the SIZE_LIMIT option to 250 MB."
      "Increase the size of the virtual warehouse used by Snowpipe."
      )
)

  (fact "Iceberg tables"
        '("don't support" "support")
        "table stages.")

(multi "Within a Snowflake account, permissions have been defined with custom roles and role hierarchies.
To set up column-level masking using a role in the hierarchy of the current user, what command would be used?"
      4
      '("IS_GRANTED_TO_INVOKER_ROLE")
      
      '("IS_ROLE_IN_SESSION"
      "INVOKER_ROLE"
      "CURRENT_ROLE"
      )
)

(multi "To fix bytes spilled to local storage"
      4
      '("Increase the size of the virtual warehouse.")
      
      '("Add additional virtual warehouses."
      "Rewrite the query using Common Table Expressions"
      "Change the order of the joins and start with smaller tables first."
      )
)

(multi "A company has an extensive script in Scala that transforms data by leveraging DataFrames. A Data Engineer needs to move these transformations to Snowpark.
What characteristics of data transformations in Snowpark should be considered to meet this requirement? (Choose two.)"
      5
      '("It is possible to join multiple tables using DataFrames."
      "Snowpark operations are executed lazily on the server.")
      
      '("User-Defined Functions (UDFs) are not pushed down to Snowflake."
      "Snowpark requires a separate cluster outside of Snowflake for computations."
      "Columns in different DataFrames with the same name should be referred to with squared brackets."
      )
)

(multi "What built-in Snowflake features make use of the change tracking metadata for a table? (Choose two.)"
      5
      '("The CHANGES clause"
      "A STREAM object")
      
      '("The MERGE command"
      "The UPSERT command"
      "The CHANGE_DATA_CAPTURE command"
      )
)

(multi "Streams cannot be created to query change data on which of the following objects?"
      4
      '("Query Log Tables")
      
      '("External tables"
      "Views, including secure views"
      "Directory tables"
      )
)

(multi "Which System Function can be used by Data engineer to verify whether a stream contains changed data for a table?"
      4
      '("SYSTEM$STREAM_HAS_DATA")
      
      '("SYSTEM$STREAM_HAS_CHANGE_DATA"
      "SYSTEM$STREAM_CDC_DATA"
      "SYSTEM$STREAM_DELTA_DATA"
      )
)


(multi "A company is building a dashboard for thousands of Analysts. The dashboard presents the results of a few summary queries on tables that are regularly updated. The query conditions vary by topic according to what data each Analyst needs. Responsiveness of the dashboard queries is a top priority, and the data cache should be preserved.
How should the Data Engineer configure the compute resources to support this dashboard?"
      4
      '("Assign all queries to a multi-cluster virtual warehouse set to maximized mode. Monitor to determine the smallest suitable number of clusters.")
      
      '("Assign queries to a multi-cluster virtual warehouse with economy auto-scaling. Allow the system to automatically start and stop clusters according to demand."
      "Create a virtual warehouse for every 250 Analysts. Monitor to determine how many of these virtual warehouses are being utilized at capacity."
      "Create a size XL virtual warehouse to support all the dashboard queries. Monitor query runtimes to determine whether the virtual warehouse should be resized."
      )
)


(multi "Which Snowflake objects does the Snowflake Kafka connector use? (Choose three.)"
      6
      '("Pipe"
      "Internal table stage"
      "Internal named stage")
      
      '("Serverless task"
      "Internal user stage"
      "Storage integration"
      )
)

  (multi "You are the owner of a table T1 which is in schema S1. The schema is in database D1. In order to grant read-only permissions of this table to a newly created role R1, you will need to…(select all that apply)"
         6
        '("Grant ‘USAGE’ on database D1"
          "Grant ‘USAGE’ on schema S1"
          "Grant ‘SELECT’ on table T1")
        
         '("Grant ‘SELECT’ on database D1"
           "Grant ‘SELECT’ on schema S1"
           "Grant ‘USAGE’ on table T1"
           )
)


   (multi "Which are characteristics of Snowpipe"
         6
        '("Load history is stored for 14 days."
        "Load history must be requested from Snowflake via a REST endpoint, SQL table function, or ACCOUNT_USAGE view."
        "Loads are combined or split into a single or multiple transactions based on the number and size of the rows in each data file"
        "Uses Snowflake-supplied compute resources.

")
         '("Load history is stored for 64 days."
           "Load history is available upon completion of the COPY statement as the statement output."
           "Loads are always performed in a single transaction."
           "Requires a user-specified warehouse to execute COPY statements."
           )
)

   (multi "Which are characteristics of batch data loads"
         6

      '("Load history is stored for 64 days."
           "Load history is available upon completion of the COPY statement as the statement output."
           "Loads are always performed in a single transaction."
           "Requires a user-specified warehouse to execute COPY statements."
           )
        '("Load history is stored for 14 days."
        "Load history must be requested from Snowflake via a REST endpoint, SQL table function, or ACCOUNT_USAGE view."
        "Loads are combined or split into a single or multiple transactions based on the number and size of the rows in each data file"
        "Uses Snowflake-supplied compute resources."
        )

)
   (fact "Clustering key consists of one or more table columns/expressions, which can be of any data type,"
         '("except" "including")
         " GEOGRAPHY, VARIANT, OBJECT, or ARRAY")


   (fact "Clustering keys"
         '("cannot" can"")
         "be defined for hybrid tables.")

  (multi "This column contains the Kafka message."
         4
        '("RECORD_CONTENT")
        
         '("RECORD_MESSAGE"
           "RECORD_DATA"
           "RECORD_METADATA"
           )
)

  (multi "Which format(s) can be used for Kafka messages?"
         4
        '("Arvo"
          "JSON")
        
         '("Parquet"
           "CSV"
           "ORC"
           "XML"
           )
)


(multi "What are characteristics of Snowpark Python packages? (Choose three.)"
      6
      '("Third-party packages can be registered as a dependency to the Snowpark session using the session.import() method."
      "The SQL command DESCRIBE FUNCTION will list the imported Python packages of the Python User-Defined Function (UDF)."
      "Querying information_schema.packages will provide a list of supported Python packages and versions."
      )
      
      '("Python packages can access any external endpoints."
      "Python packages can only be loaded in a local environment."
      "Third-party supported Python packages are locked down to prevent hitting."
      )
)

(multi "While running an external function, the following error message is received:
Error: Function received the wrong number of rows
What is causing this to occur?"
      4
      '("The return message did not produce the same number of rows that it received")
      
      '("The JSON returned by the remote service is not constructed correctly."
      "Nested arrays are not supported in the JSON response."
      "External functions do not support multiple rows."
      )
)


(multi "A Data Engineer is building a pipeline to transform a 1 TB table by joining it with supplemental tables. The Engineer is applying filters and several aggregations leveraging Common Table Expressions (CTEs) using a size Medium virtual warehouse in a single query in Snowflake.
What is the recommended approach to MAXIMIZE performance of this query if the Profile shows data spillage?"
      4
      '("Increase the warehouse size.")
      
      '("Enable clustering on the table."
      "Rewrite the query to remove the CTEs."
      "Switch to a multi-cluster virtual warehouse."
      )
)

  (multi "Snowflake provides system data metric function (DMFs) in which schema of the shared SNOWFLAKE database?"
         4
        '("CORE")
        
         '("INFORMATION_SCHEMA"
           "DMF"
           "TOOLS"
           )
)

  (multi "Which privilege enables you to control which roles have access to serverless compute resources to call the system data metric function (DMF)?"
         4
        '("EXECUTE DATA METRIC FUNCTION")
        
         '("CALL SERVERLESS COMPUTE"
           "EXECUTE SERVERLESS COMPUTE"
           "CALL DATA METRIC FUNCTION"
           )
)

  (multi "Which use case would be BEST suited for the search optimization service?"
         4
        '("Business users who need fast response times using highly selective filters.")
        
         '("Analysts who need to perform aggregates over high-cardinality columns."
           "Data Scientists who seek specific JOIN statements with large volumes of data."
           "Data Engineers who create clustered tables with frequent reads against clustering keys."
           )
)

  (multi "What is a characteristic of the use of binding variables in JavaScript stored procedures in Snowflake?"
         4
        '("Only JavaScript variables of type number, string, and SfDate can be bound.")
        
         '("All types of JavaScript variables can be bound."
           "All Snowflake first-class objects can be bound."
           "Users are restricted from binding JavaScript variables because they create SQL injection attack vulnerabilities."
           )
)

  (multi "Which system role is recommended for a custom role hierarchy to be ultimately assigned to?"
         4
        '("SYSADMIN")
        
         '("USERADMIN"
           "SECURITYADMIN"
           "ACCOUNTADMIN"
           )
)

  (multi "At what isolation level are Snowflake streams?"
         4
        '("Repeatable read")
        
         '("Snapshot"
           "Read committed"
           "Read uncommitted"
           )
)

  (multi "Streams support repeatable read isolation. In repeatable read mode"
         2
        '("multiple SQL statements within a transaction see the same set of records in a stream")
        
         '("statements see any changes made by previous statements executed within the same transaction, even though those changes are not yet committed."
           )
)

  (multi "Which methods can be used to create a DataFrame object in Snowpark? (Choose three.)"
         6
        '("session.read.json()"
          "session.table()"
          "session.sql()")
        
         '("session.jdbc_connection()"
           "DataFrame.write()"
           "session.builder()"
           )
)

(multi "A Data Engineer wants to create a new development database (DEV) as a clone of the permanent production database (PROD). There is a requirement to disable Fail-safe for all tables.
Which command will meet these requirements?"
      4
      '("CREATE TRANSIENT DATABASE DEV -
CLONE PROD;")
      
      '("CREATE DATABASE DEV -
CLONE PROD -
FAIL_SAFE = FALSE;"
      "CREATE DATABASE DEV -
CLONE PROD;"
      "CREATE DATABASE DEV -
CLONE PROD -
DATA_RETENTION_TIME_IN DAYS = 0;"
      )
)

(fact "Renaming a source object"
      '("does not break a stream or cause" "breaks a stream and causes")
      "it to go stale.")

(fact "Streams support repeatable read isolation, which means"
      '("multiple SQL statements within a transaction see the same set of records in a stream" 
      "statements see any changes made by previous statements executed within the same transaction, even though those changes are not yet committed")
      nil)

(fact "If a source object is dropped and a new object is created with the same name, any streams linked to the original object"
      '("are not" "are")
      "linked to the new object.")



  (multi "A Data Engineer executes a complex query and wants to make use of Snowflake’s query results caching capabilities to reuse the results.
Which conditions must be met? (Choose three.)"
         6
        '("The table structure contributing to the query result cannot have changed."
          "The new query must have the same syntax as the previously executed query."
          "The micro-partitions cannot have changed due to changes to other data in the table.")
        
         '("The results must be reused within 72 hours."
           "The query must be executed using the same virtual warehouse."
           "The USED_CACHED_RESULT parameter must be included in the query."
           )
)

  (multi "By default, the privileges required to create and manage shares are granted only to the"
         4
        '("ACCOUNTADMIN")
        
         '("SYSADMIN"
           "USERADMIN"
           "SECURITYADMIN"
           )
)
 
  (multi "Steam columns include:"
         4
        '("METADATA$ACTION"
          "METADATA$ISUPDATE"
          "METADATA$ROW_ID")
        
         '("METADATA$DML"
           "METADATA$TYPE"
           "METADATA$ACTION_DATE"
           )
)
        


  (multi "Both synchronous and asynchronous queries allowed using the Python Connector?"
         4
        '("True")
        
         '("False. Neither are allowed"
           "False. Only synchronous are allowed."
           "False. Only asynchronous are allowed."
           )
)

  (multi "What is a characteristic of the operations of streams in Snowflake?"
         4
        '("When a stream is used to update a target table, the offset is advanced to the current time.")
         '("Whenever a stream is queried, the offset is automatically advanced."
           "Querying a stream returns all change records and table rows from the current offset to the current time."
           "Each committed and uncommitted transaction on the source table automatically puts a change record in the stream."
           )
)

  (multi "A Data Engineer wants to centralize grant management to maximize security. A user needs OWNERSHIP on a table in a new schema. However, this user should not have the ability to make grant decisions.
What is the correct way to do this?"
         4
        '("Add the WITH MANAGED ACCESS parameter on the schema.")
        
         '("Grant OWNERSHIP to the user on the table."
           "Revoke grant decisions from the user on the table."
           "Revoke grant decisions from the user on the schema."
           )
)

  (multi "A Data Engineer is implementing a near real-time ingestion pipeline to load data into Snowflake using the Snowflake Kafka connector. There will be three Kafka topics created.
Which Snowflake objects are created automatically when the Kafka connector starts?"
         4
        '("Tables"
          "Pipes"
          "Internal stages")
        
         '("Tasks"
           "External stages"
           "Materialized views"
           )
)

  (multi "What kind of Snowflake integration is required when defining an external function in Snowflake?"
         4
        '(API integration"")
        
         '("HTTP integration"
           "Notification integration"
           "Security integration"
           )
)

  (multi "What is the purpose of the BUILD_STAGE_FILE_URL function in Snowflake?"
         4
        '("It generates a permanent URL for accessing files in a stage.")
        
         '("It generates an encrypted URL for accessing a file in a stage."
           "It generates a staged URL for accessing a file in a stage."
           "It generates a temporary URL for accessing a file in a stage."
           )
)

  (multi "A Data Engineer needs to load JSON output from some software into Snowflake using Snowpipe.
Which recommendation(s) apply to this scenario?"
         4
        '("Ensure that data files are 100-250 MB (or larger) in size, compressed."
         "Extract semi-structured data elements containing null values into relational columns before loading."
         "Verify each value of each unique element stores a single native data type (string or number).")
        
         '("Load large files (1 GB or larger)."
           "Load a single huge array containing multiple records into a single table row."
           "Create data files that are less than 100 MB and stage them in cloud storage at a sequence greater than once each minute."
           )
)

  (multi "Which Snowflake feature facilitates access to external API services such as geocoders, data transformation, machine learning models, and other custom code?"
         4
        '("External functions")
        
         '("Security integration"
           "External tables"
           "Java User-Defined Functions (UDFs)"
           )
)

(fact "For external functions. the remote service"
      '("must" "doesn't have to")
      "return one row for each row received."
      )

(fact "RETURNS NULL ON NULL INPUT"
      '("will not" "will")
      "call the User-Defined Function (UDF) if any input is null."
      )


  (multi "A Data Engineer is working on a Snowflake deployment in AWS eu-west-1 (Ireland). The Engineer is planning to load data from staged files into target tables using the COPY INTO command.
Which sources are valid? (Choose 3)"
         6
        '("External stage in an Amazon S3 bucket on AWS eu-central-1 (Frankfurt)"
          "External stage on GCP us-central1 (Iowa)"
          "External stage in an Amazon S3 bucket on AWS eu-west-1 (Ireland)"
          )
        
         '("Internal stage on GCP us-central1 (Iowa)"
           "Internal stage on AWS eu-central-1 (Frankfurt)"
           "SSD attached to an Amazon EC2 instance on AWS eu-west-1 (Ireland)"
           )
)

  (multi "A Data Engineer needs to know the details regarding the micro-partition layout for a table named Invoice using a built-in function.
Which query will provide this information?"
         4
        '("SELECT SYSTEM$CLUSTERING_INFORMATION('Invoice');")
        
         '("SELECT $CLUSTERING_INFORMATION('Invoice');"
           "CALL SYSTEM$CLUSTERING_INFORMATION('Invoice');"
           "CALL $CLUSTERING_INFORMATION('Invoice');"
           )
)

  (multi "Assuming a Data Engineer has all appropriate privileges and context, which statements would be used to assess whether the User-Defined Function (UDF), MYDATABASE.SALES.REVENUE_BY_REGION, exists and is secure? (Choose two.)"
         5
        '("SHOW USER FUNCTIONS LIKE 'REVENUE_BY_REGION' IN SCHEMA SALES;"
          "SELECT IS_SECURE FROM INFORMATION_SCHEMA.FUNCTIONS WHERE FUNCTION_SCHEMA = 'SALES' AND FUNCTION_NAME = 'REVENUE_BY_REGION';")
        
         '("SELECT IS_SECURE FROM SNOWFLAKE.INFORMATION_SCHEMA.FUNCTIONS WHERE FUNCTION_SCHEMA = 'SALES' AND FUNCTION_NAME = 'REVENUE_BY_REGION';"
           "SHOW EXTERNAL FUNCTIONS LIKE 'REVENUE_BY_REGION' IN SCHEMA SALES;"
           "SHOW SECURE FUNCTIONS LIKE 'REVENUE_BY_REGION' IN SCHEMA SALES;"
           )
)
  
  (multi "A Data Engineer is writing a Python script using the Snowflake Connector for Python. The Engineer will use the snowflake.connector.connect function to connect to Snowflake.
The requirements are:
1. Raise an exception if the specified database, schema, or warehouse does not exist
2. Improve download performance
Which parameters of the connect function should be used? (Choose two.)
"
         5
        '("client_prefetch_threads"
          "validate_default_parameters")
        
         '("authenticator"
           "arrow_number_to_decimal"
           "client_session_keep_alive"
           )
)


  (multi "How can a relational data be transformed into semi-structured data using the LEAST amount of operational overhead?"
         4
        '("Use the OBJECT_CONSTRUCT function to return a Snowflake object.")
        
         '("Use the PARSE_JSON function to produce a VARIANT value."
           "Use the TO_JSON function."
           "Use the TO_VARIANT function to convert each of the relational columns to VARIANT."
           )
)

  (multi "A Data Engineer ran a stored procedure containing various transactions. During the execution, the session abruptly disconnected, preventing one transaction from committing or rolling back. The transaction was left in a detached state and created a lock on resources.
What step must the Engineer take to immediately run a new transaction?"
         4
        '("Call the system function SYSTEM$ABORT_TRANSACTION.")
        
         '("Call the system function SYSTEM$CANCEL_TRANSACTION."
           "Set the LOCK_TIMEOUT to FALSE in the stored procedure."
           "Set the TRANSACTION_ABORT_ON_ERROR to TRUE in the stored procedure."
           )
)

  (multi "Which methods will trigger an action that will evaluate a DataFrame? (Choose two.)"
         5
        '("DataFrame.collect()"
          "DataFrame.show()")
        
         '("DataFrame.random_split()"
           "DataFrame.select()"
           "DataFrame.col()"
           )
)

  (multi "Given the table SALES which has a clustering key of column CLOSED_DATE, which table function will return the average clustering depth for the SALES_REPRESENTATIVE column for the North American region?"
         4
        '("select system$clustering_depth('Sales', 'sales_representative', 'region = ''North America''');")
        
         '("select system$clustering_information('Sales', 'sales_representative', 'region = ''North America''');"
           "select system$clustering_depth('Sales', 'sales_representative') where region = 'North America';"
           "select system$clustering_information('Sales', 'sales_representative') where region = 'North America’;"
           )
)

  (multi "A new CUSTOMER table is created by a data pipeline in a Snowflake schema where MANAGED ACCESS is enabled.
Which roles can grant access to the CUSTOMER table? (Choose three.)"
         6
        '("The role that owns the schema"
          "The SECURITYADMIN role"
          "The USERADMIN role with the MANAGE GRANTS privilege")
        
         '("The role that owns the database"
           "The SYSADMIN role"
           "The role that owns the CUSTOMER table"
           )
)

  (multi "You are designing storage for 20 TB of text files as part of deploying a data pipeline on Google Cloud. Your input data is in CSV format. You want to minimize the cost of querying aggregate values for multiple users who will query the data in Cloud Storage with multiple engines. Which storage service and schema design should you use?"
         4
        '("Use Cloud Storage for storage. Link as permanent tables in BigQuery for query.") 
         '("Use Cloud Bigtable for storage. Install the HBase shell on a Compute Engine instance to query the Cloud Bigtable data."
           "Use Cloud Bigtable for storage. Link as permanent tables in BigQuery for query."
           "Use Cloud Storage for storage. Link as temporary tables in BigQuery for query."
           )
)



;; GET_STAGE_LOCATION -- converst stage name into cloud URL (i.e. gcs://...)
;; GET_ABSOLUTE_PATH
;; GET_RELATIVE_PATH  -- removes the bucket/path-prefix from an abs URL
;; GET_PRESIGNED_URL
;;
;; Scoped URL expiry
;; BUILD_STAGE_FILE_URI
;; SNOWFLAKE_SSE?

