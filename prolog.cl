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
  (fact "When an object is cloned"
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

  (fact "In a managed schema only the"
        '("schema" "object")
        "owner can grant privileged on contained objects")

  (fact "In an unmanaged schema only the"
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
      '("immediately moves data in Time Trabel that is outside of the new retention period into failsafe"
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
      '("proviudes access without needing ANY privileges or external authentication"
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

;; GET_STAGE_LOCATION -- converst stage name into cloud URL (i.e. gcs://...)
;; GET_ABSOLUTE_PATH
;; GET_RELATIVE_PATH  -- removes the bucket/path-prefix from an abs URL
;; GET_PRESIGNED_URL
;;
;; Scoped URL expiry
;; BUILD_STAGE_FILE_URI
;; SNOWFLAKE_SSE?

