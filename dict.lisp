;; JMdict crawler

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"foobar")

(defparameter *jmdict-data* #p"foobar")

(defparameter *connection* '("jmdict" "postgres" "" "localhost"))

(eval-when (:load-toplevel)
  (load (asdf:system-relative-pathname :ichiran "settings.lisp") :if-does-not-exist nil))

(defgeneric get-kana (obj)
  (:documentation "most popular kana representation"))

(defgeneric get-text (obj)
  (:documentation "most popular text representation (kanji or kana)")
  (:method (obj) (text obj)))

(defclass entry ()
  ((seq :reader seq :col-type integer :initarg :seq)
   (content :reader content :col-type string :initarg :content)
   (root-p :reader root-p :col-type boolean :initform nil :initarg :root-p)
   (n-kanji :accessor n-kanji :col-type integer :initform 0 :initarg :n-kanji)
   (n-kana :accessor n-kana :col-type integer :initform 0 :initarg :n-kana)
   )
  (:metaclass dao-class)
  (:keys seq))

(deftable entry
  (!dao-def))

(defmethod print-object ((obj entry) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a:~a" (seq obj) (n-kanji obj) (n-kana obj))))

(defmethod get-kana ((obj entry))
  (text (car (select-dao 'kana-text (:and (:= 'seq (seq obj)) (:= 'ord 0))))))

(defmethod get-text ((obj entry))
  (text (car (select-dao (if (> (n-kanji obj) 0) 'kanji-text 'kana-text)
                         (:and (:= 'seq (seq obj)) (:= 'ord 0))))))

(defun recalc-entry-stats ()
  (query (:update 'entry :set
                  'n-kanji (:select (:count 'id) :from 'kanji-text :where (:= 'kanji-text.seq 'entry.seq))
                  'n-kana (:select (:count 'id) :from 'kana-text :where (:= 'kana-text.seq 'entry.seq)))))

(defclass kanji-text ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (common :reader common :col-type (or db-null integer) :initarg :common)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable kanji-text
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!index 'text)
  (!index 'common)
  (!foreign 'entry 'seq))

(defmethod print-object ((obj kanji-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (seq obj) (text obj))))

(defmethod get-kana ((obj kanji-text))
  (text (car (select-dao 'kana-text (:and (:= 'seq (seq obj)) (:= 'ord 0))))))

(defclass kana-text ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (common :reader common :col-type (or db-null integer) :initarg :common)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable kana-text
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!index 'text)
  (!index 'common)
  (!foreign 'entry 'seq))

(defmethod print-object ((obj kana-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (seq obj) (text obj))))

(defmethod get-kana ((obj kana-text))
  (text obj))

(defclass sense ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (ord :reader ord :col-type integer :initarg :ord))
  (:metaclass dao-class)
  (:keys id))

(deftable sense
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!foreign 'entry 'seq))

(defclass gloss ()
  ((id :reader id :col-type serial)
   (sense-id :reader sense-id :col-type integer :initarg :sense-id)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   )
  (:documentation "English meaning")
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj gloss) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "(~a) ~a" (ord obj) (text obj))))

(deftable gloss
  (!dao-def)
  (!index 'sense-id)
  (!index 'ord)
  (!foreign 'sense 'sense-id 'id))

(defclass sense-prop ()
  ((id :reader id :col-type serial)
   (tag :reader tag :col-type string :initarg :tag)
   (sense-id :reader sense-id :col-type integer :initarg :sense-id)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (seq :reader seq :col-type integer :initarg :seq)
   )
  (:documentation "sense properties")
  (:metaclass dao-class)
  (:keys id))
  
(defmethod print-object ((obj sense-prop) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a:~a" (tag obj) (text obj))))

(deftable sense-prop
  (!dao-def)
  (!index 'sense-id)
  (!index 'tag)
  (!index 'text)
  (!index 'ord)
  (!index 'seq)
  (!foreign 'entry 'seq)
  (!foreign 'sense 'sense-id 'id))

(defclass conjugation ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (from :reader seq-from :col-type integer :initarg :from)
   )
  (:documentation "conjugation link")
  (:metaclass dao-class)
  (:keys id))

(deftable conjugation
  (!dao-def)
  (!index 'seq)
  (!index 'from)
  (!foreign 'entry 'seq)
  (!foreign 'entry 'from 'seq))

(defmethod print-object ((obj conjugation) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a -> ~a" (seq-from obj) (seq obj))))

(defclass conj-prop ()
  ((id :reader id :col-type serial)
   (conj-id :reader conj-id :col-type integer :initarg :conj-id)
   (conj-type :reader conj-type :col-type integer :initarg :conj-type)
   (pos :reader pos :col-type string :initarg :pos)
   (neg :reader conj-neg :col-type (or db-null boolean) :initarg :neg)
   (fml :reader conj-fml :col-type (or db-null boolean) :initarg :fml))
  (:metaclass dao-class)
  (:keys id))

(deftable conj-prop
  (!dao-def)
  (!index 'conj-id)
  (!foreign 'conjugation 'conj-id 'id))

(defun conj-info-short (obj)
  (format nil "[~a] ~a~[ Affirmative~; Negative~]~[ Plain~; Formal~]"
          (pos obj) 
          (get-conj-description (conj-type obj))
          (case (conj-neg obj) ((nil) 0) ((t) 1))
          (case (conj-fml obj) ((nil) 0) ((t) 1))
          ))

(defmethod print-object ((obj conj-prop) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (princ (conj-info-short obj) stream)))

(defstruct conj-data seq from prop)

(defun get-conj-data (seq)
  (loop for conj in (select-dao 'conjugation (:= 'seq seq))
       nconcing (loop for prop in (select-dao 'conj-prop (:= 'conj-id (id conj)))
                     collect (make-conj-data :seq (seq conj) :from (seq-from conj)
                                             :prop prop))))

(defun init-tables ()
  (with-connection *connection*
    (let ((tables '(entry kanji-text kana-text sense gloss sense-prop conjugation conj-prop)))
      (loop for table in (reverse tables)
         do (query (:drop-table :if-exists table)))
      (loop for table in tables
         do (create-table table)))))

;; Taken from webgunk so that ichiran doesn't depend on it
;; strip-whitespace option is removed as it doesn't look necessary in this case
(defun node-text (node &rest args &key test)
  (let (values result)
    (when (or (not test) (funcall test node))
      (dom:do-node-list (node (dom:child-nodes node))
        (let ((val (case (dom:node-type node)
                     (:element (apply #'node-text node args))
                     (:text (dom:node-value node)))))
          (push val values))))
    (setf result (apply #'concatenate 'string (nreverse values)))))

(defmacro do-node-list-ord ((ord-var node-var node-list) &body body)
  `(let ((,ord-var 0))
     (dom:do-node-list (,node-var ,node-list)
       ,@body
       (incf ,ord-var))))

(defun insert-readings (node-list tag table seq pri)
  (do-node-list-ord (ord node node-list)
    (let* ((reading-node (dom:item (dom:get-elements-by-tag-name node tag) 0))
           (reading-text (node-text reading-node))
           (common :null))
      (dom:do-node-list (node (dom:get-elements-by-tag-name node pri))
        (let ((pri-tag (node-text node)))
          (if (eql common :null) (setf common 0))
          (when (alexandria:starts-with-subseq "nf" pri-tag)
            (setf common (parse-integer pri-tag :start 2)))))
      (make-dao table :seq seq :text reading-text :ord ord :common common))))

(defun insert-sense-traits (sense-node tag sense-id seq)
  (do-node-list-ord (ord node (dom:get-elements-by-tag-name sense-node tag))
    (make-dao 'sense-prop :sense-id sense-id :tag tag :text (node-text node) :ord ord :seq seq)))

(defun insert-senses (node-list seq)
  (do-node-list-ord (ord node node-list)
    (let ((sense-id (id (make-dao 'sense :seq seq :ord ord))))
      (do-node-list-ord (ord node (dom:get-elements-by-tag-name node "gloss"))
        (make-dao 'gloss :sense-id sense-id :text (node-text node) :ord ord))
      (insert-sense-traits node "pos" sense-id seq)
      (insert-sense-traits node "misc" sense-id seq)
      (insert-sense-traits node "dial" sense-id seq))))

(defun load-entry (content)
  (let* ((parsed (cxml:parse content (cxml-dom:make-dom-builder)))
         (entseq-node (dom:item (dom:get-elements-by-tag-name parsed "ent_seq") 0))
         (seq (parse-integer (node-text entseq-node))))
    (make-dao 'entry :seq seq :content content :root-p t)
    (let* ((kanji-nodes (dom:get-elements-by-tag-name parsed "k_ele"))
           (kana-nodes (dom:get-elements-by-tag-name parsed "r_ele"))
           (sense-nodes (dom:get-elements-by-tag-name parsed "sense")))
      (insert-readings kanji-nodes "keb" 'kanji-text seq "ke_pri")
      (insert-readings kana-nodes "reb" 'kana-text seq "re_pri")
      (insert-senses sense-nodes seq))))

(defun fix-entities (source)
  "replaces entity definitions with abbreviations"
  (let ((entity-hash (cxml::dtd-gentities (cxml::dtd (slot-value source 'cxml::context)))))
    (maphash 
     (lambda (name entdef)
       (unless (member name '("lt" "gt" "amp" "apos" "quot") :test 'equal)
         (setf (cxml::entdef-value (cdr entdef)) name)))
     entity-hash)))
    
(defun load-jmdict (&key (path *jmdict-path*) (load-conjugations t))
  (init-tables)
  (with-connection *connection*
    (klacks:with-open-source (source (cxml:make-source path))
      (klacks:find-element source "JMdict")
      (fix-entities source)
      (loop for cnt from 1 ;; to 1000
         while (klacks:find-element source "entry")
         do
           (let ((content (klacks:serialize-element source (cxml:make-string-sink))))
             (load-entry content))
         if (zerop (mod cnt 1000)) do (format t "~a entries loaded~%" cnt)
         finally (query "ANALYZE") (format t "~a entries total~%" cnt)))
    (when load-conjugations
      (format t "Loading conjugations...~%")
      (load-conjugations))
    (add-errata)
    (recalc-entry-stats)
    (query "ANALYZE")
    ))

;;; conjugations generator (warning: terrible code ahead)

(defmacro csv-hash (hash-name (filename &key skip-first) loader-opts &rest accessor-opts-list)
  (let ((base-name (string-trim "*" hash-name))
        (forms (list `(defparameter ,hash-name nil)))
        (loader-opts-length (length loader-opts)) ;;([loader-name] row-def row-key value-form)
        loader-name
        (row-count-var (gensym "ROW"))
        )
    (assert (member loader-opts-length '(3 4)))
    (setf loader-name
          (if (= loader-opts-length 4)
              (pop loader-opts)
              (intern (concatenate 'string (symbol-name :load-) base-name))))
    (destructuring-bind (row-def row-key-form value-form) loader-opts
      (push
       `(defun ,loader-name ()
          (setf ,hash-name (make-hash-table :test 'equal))
          (loop :for ,row-def :in (cl-csv:read-csv 
                                   (merge-pathnames *jmdict-data* ,filename)
                                   :separator #\Tab :skip-first-p ,skip-first)
             :for ,row-count-var :from 0
             :do (setf (gethash ,row-key-form ,hash-name) ,value-form)))
       forms))
    (loop with accessor-name
       for accessor-opts in accessor-opts-list
       for accessor-opts-length = (length accessor-opts) ;; ([accessor-name] val-var val-var-form)
       for param = (gensym "KEY")
       do (assert (member accessor-opts-length '(2 3)))
         (setf accessor-name
               (if (= accessor-opts-length 3)
                   (pop accessor-opts)
                   (intern (concatenate 'string (symbol-name :get-) base-name))))
         (destructuring-bind (val-var val-var-form) accessor-opts
           (push
            `(defun ,accessor-name (,param)
               (unless ,hash-name (,loader-name))
               (let ((,val-var (gethash ,param ,hash-name)))
                 ,val-var-form))
            forms)))
    `(progn ,@(nreverse forms))))
               
(csv-hash *pos-index* ("kwpos.csv")
          ((pos-id pos description) pos (cons (parse-integer pos-id) description))
          (val (car val)))

(csv-hash *pos-by-index* ("kwpos.csv")
          ((pos-id pos description) (parse-integer pos-id) pos)
          (get-pos val val))

(csv-hash *conj-description* ("conj.csv" :skip-first t)
          ((conj-id description) (parse-integer conj-id) description)
          (val val))

(defstruct (conjugation-rule
             (:conc-name cr-)
             (:constructor make-conjugation-rule (pos conj neg fml onum stem okuri euphr euphk)))
  pos conj neg fml onum stem okuri euphr euphk)


(csv-hash *conj-rules* ("conjo.csv" :skip-first t)
          ((pos-id conj-id neg fml onum stem okuri euphr euphk pos2)
           (parse-integer pos-id)
           (let ((pos (parse-integer pos-id)))
             (cons (make-conjugation-rule pos
                                          (parse-integer conj-id)
                                          (case (char neg 0) (#\t t) (#\f nil))
                                          (case (char fml 0) (#\t t) (#\f nil))
                                          (parse-integer onum)
                                          (parse-integer stem)
                                          okuri euphr euphk)
                   (gethash pos *conj-rules* nil))))
          (val (reverse val)))


(defun construct-conjugation (word rule)
  (let* ((iskana (test-word (subseq word (max 0 (- (length word) 2))) :kana))
         (euphr (cr-euphr rule))
         (euphk (cr-euphk rule))
         (stem (+ (cr-stem rule)
                  (if (or (and iskana (> (length euphr) 0))
                          (and (not iskana) (> (length euphk) 0)))
                      1 0))))
    (concatenate 'string (subseq word 0 (- (length word) stem))
                 (if iskana euphr euphk)
                 (cr-okuri rule))))

(defun conjugate-word (word pos)
  (let* ((pos-id (get-pos-index pos))
         (rules (get-conj-rules pos-id)))
    (loop for rule in rules
         collect (cons rule (construct-conjugation word rule)))))

(defun get-all-readings (seq)
  (query (:union
          (:select 'text :from 'kanji-text :where (:= 'seq seq))
          (:select 'text :from 'kana-text :where (:= 'seq seq)))
         :column))

(defparameter *do-not-conjugate* '("n" "vs" "adj-na"))

(defun conjugate-entry-inner (seq)
  (let ((posi (query (:select 'text :distinct :from 'sense-prop
                              :where (:and (:= 'tag "pos") (:= 'seq seq))) :column)))
    (loop with conj-matrix = (make-hash-table :test 'equal) ;; (cons pos-id conj-id) -> 2x2 array
       for pos in posi
       for pos-id = (get-pos-index pos)
       for rules = (get-conj-rules pos-id)
       if (and rules (not (member pos *do-not-conjugate* :test 'equal)))
         do (loop 
               for (reading ord kanji-flag) in (query (:union (:select 'text 'ord 1 :from 'kanji-text :where (:= 'seq seq))
                                                              (:select 'text 'ord 0 :from 'kana-text :where (:= 'seq seq))))
               do (loop for rule in rules
                       for conj-id = (cr-conj rule)
                       for key = (list pos-id conj-id)
                       for conj-text = (construct-conjugation reading rule)
                     do (unless (gethash key conj-matrix)
                          (setf (gethash key conj-matrix)
                                (make-array '(2 2) :initial-element nil)))
                       (push (list conj-text kanji-flag ord (cr-onum rule))
                             (aref (gethash key conj-matrix)
                                   (if (cr-neg rule) 1 0)
                                   (if (cr-fml rule) 1 0)))))
         finally (return conj-matrix))))

(defun conjugate-entry-outer (seq)
  (let* ((conj-matrix (conjugate-entry-inner seq))
         (original-readings (get-all-readings seq))
         (max-seq (query (:select (:max 'seq) :from 'entry) :single))
         (next-seq (1+ max-seq)))
    (loop for (pos-id conj-id) being the hash-key of conj-matrix using (hash-value matrix)
       for ignore-neg = (not (or (aref matrix 1 0) (aref matrix 1 1)))
       for ignore-fml = (not (or (aref matrix 0 1) (aref matrix 1 1)))
       for pos = (get-pos pos-id)
         do (loop for ii from 0 below 4
               for neg = (>= ii 2)
               for fml = (oddp ii)
               for readings = (remove-if (lambda (item) 
                                           (member (car item) original-readings :test 'equal))
                                         (row-major-aref matrix ii))
               when readings
               do (when (insert-conjugation readings :seq next-seq
                                            :from seq :pos pos
                                            :conj-id conj-id
                                            :neg (if ignore-neg :null neg)
                                            :fml (if ignore-fml :null fml))
                    (incf next-seq))))))

(defun lex-compare (predicate)
  "Only can sort sequences of equal length"
  (lambda (seq1 seq2)
    (block nil
      (map nil 
           (lambda (e1 e2) 
             (cond ((funcall predicate e1 e2) (return t))
                   ((funcall predicate e2 e1) (return nil))))
           seq1 seq2))))

(defun insert-conjugation (readings &key seq from pos conj-id neg fml)
  "returns true if new entry is created, nil otherwise"
  (loop for (reading kanji-flag) in (sort readings (lex-compare #'<) :key #'cddr)
     if (= kanji-flag 1) collect reading into kanji-readings
     else collect reading into kana-readings
     finally
       (unless kana-readings (return nil))
       (let* ((kanji-readings (remove-duplicates kanji-readings :test 'equal))
              (kana-readings (remove-duplicates kana-readings :test 'equal))
              (seq-candidates
               (if kanji-readings
                   (query (:intersect 
                           (:select 'seq :from 'kanji-text
                                    :where (:in 'text (:set kanji-readings))
                                    :group-by 'seq
                                    :having (:= (:count 'id) (length kanji-readings)))
                           (:select 'seq :from 'kana-text
                                 :where (:in 'text (:set kana-readings))
                                 :group-by 'seq
                                 :having (:= (:count 'id) (length kana-readings))))
                          :column)
                   (query (:select 'r.seq
                                   :from (:as 'kana-text 'r)
                                   :left-join (:as 'kanji-text 'k) :on (:= 'r.seq 'k.seq)
                                   :where (:and (:is-null 'k.text)
                                                (:in 'r.text (:set kana-readings)))
                                   :group-by 'r.seq
                                   :having (:= (:count 'r.id) (length kana-readings)))
                          :column))))
         (when (member from seq-candidates)
           (return nil))
         (if seq-candidates
             (setf seq (car seq-candidates))
             (progn
               (make-dao 'entry :seq seq :content "")
               (loop for kr in kanji-readings
                  for ord from 0
                  do (make-dao 'kanji-text :seq seq :text kr :ord ord :common :null))
               (loop for kr in kana-readings
                  for ord from 0
                  do (make-dao 'kana-text :seq seq :text kr :ord ord :common :null))))
         
         (let* ((old-conj (select-dao 'conjugation (:and (:= 'from from) (:= 'seq seq))))
                (conj (or (car old-conj) (make-dao 'conjugation :seq seq :from from))))
           (make-dao 'conj-prop :conj-id (id conj) :conj-type conj-id :pos pos :neg neg :fml fml))
         (return (not seq-candidates)))))

(defparameter *pos-with-conj-rules*
 '("adj-i" "adj-ix" "cop-da" "v1" "v1-s" "v5aru" 
   "v5b" "v5g" "v5k" "v5k-s" "v5m" "v5n" "v5r" "v5r-i" "v5s"
   "v5t" "v5u" "v5u-s" "vk" "vs-s" "vs-i"))

(defun load-conjugations ()
  (with-connection *connection*
    (let ((seqs (query (:select 'seq :distinct :from 'sense-prop
                                :where (:and (:= 'tag "pos")
                                             (:in 'text (:set *pos-with-conj-rules*))))
                       :column)))
      (loop for cnt from 1
           for seq in seqs
           do (conjugate-entry-outer seq)
           if (zerop (mod cnt 500)) do (format t "~a entries processed~%" cnt)))))


;;; end conjugations

(defun find-word (word)
  (select-dao 
   (if (test-word word :kana) 'kana-text 'kanji-text)
   (:= 'text word)))

(defun word-readings (word)
  (let* ((kana-seq (query (:select 'seq :from 'kana-text :where (:= 'text word)) :column))
         (readings
          (if kana-seq (list word)
              (let* ((kanji-seq (query (:select 'seq :from 'kanji-text
                                                :where (:= 'text word)) :column)))
                (query (:order-by 
                        (:select 'text :from 'kana-text :where
                                 (:in 'seq (:set kanji-seq)))
                        'id) :column)))))
    (values readings (mapcar #'ichiran:romanize-word readings))))

(defstruct segment
  start end word (score nil) (info nil) (top nil)) ;; (accum 0) (path nil)

(defun calc-score (reading &optional final)
  (let* ((score 1)
         (kanji-p (typep reading 'kanji-text))
         (katakana-p (and (not kanji-p) (test-word (text reading) :katakana)))
         (len (length (text reading))))
    (with-slots (seq ord) reading
      (let* ((entry (get-dao 'entry seq))
             (conj-of (query (:select 'from :from 'conjugation :where (:= 'seq seq)) :column))
             (seq-set (if (root-p entry) (list seq) (cons seq conj-of)))
             (prefer-kana
              (select-dao 'sense-prop (:and (:in 'seq (:set seq-set)) (:= 'tag "misc") (:= 'text "uk"))))
             (posi (query (:select 'text :distinct :from 'sense-prop
                                   :where (:and (:in 'seq (:set seq-set)) (:= 'tag "pos"))) :column))
             (common (common reading))
             (common-p (not (eql common :null)))
             (particle-p (member "prt" posi :test 'equal))
             (pronoun-p (member "pn" posi :test 'equal))
             (long-p (> len (if (or kanji-p (and common-p (< 0 common 10))) 2 3)))
             (primary-p (and (= ord 0)
                             (or (and common-p pronoun-p)
                                 (and prefer-kana (not kanji-p))
                                 (and (not prefer-kana) kanji-p)
                                 (= (n-kanji entry) 0)))))
        (unless common-p
          (let* ((table (if kanji-p 'kanji-text 'kana-text))
                 (conj-of-common (query (:select 'id :from table
                                                 :where (:and (:in 'seq (:set conj-of))
                                                              (:not-null 'common)))
                                       :column)))
            (when conj-of-common
              (setf common 0 common-p t))))
        (when primary-p
          (incf score (if (or kanji-p long-p) 10 5))
          (when particle-p
            (when common-p
              (incf score 5))
            (when final
              (incf score 5))))
        (when common-p
          (if (or primary-p long-p)
              (incf score (if (= common 0) 10 (max (- 20 common) 10)))
              (incf score 2)))
        (when long-p
          (setf score (max 5 score)))
        (setf score (* score (expt len (if (or kanji-p katakana-p) 3 2))))
        (values score (list :posi posi :seq-set seq-set
                            :conj (get-conj-data seq)))))))

(defun gen-score (segment &optional final)
  (setf (values (segment-score segment) (segment-info segment))
        (calc-score (segment-word segment) final))
  segment)

(defun find-sticky-positions (str)
  "words cannot start or end after sokuon and before yoon characters"
  (loop with modifiers = (append *modifier-characters* *iteration-characters*)
     for pos from 0 below (length str)
     for char = (char str pos)
     for char-class = (gethash char *char-class-hash* char)
     if (eql char-class :sokuon) collect (1+ pos)
     else if (member char-class modifiers) collect pos))

(defun find-substring-words (str)
  (loop with sticky = (find-sticky-positions str)
       for start from 0 below (length str)
       unless (member start sticky)
       nconcing 
       (loop for end from (1+ start) upto (length str)
            unless (member end sticky)
            nconcing (mapcar 
                      (lambda (word)
                        (gen-score (make-segment :start start :end end :word word)
                                   (= end (length str))))
                      (find-word (subseq str start end))))))

(defparameter *identical-word-score-cutoff* 1/2)

(defun cull-segments (segments)
  (when segments
    (let* ((segments (stable-sort segments #'> :key #'segment-score))
           (max-score (segment-score (car segments)))
           (cutoff (* max-score *identical-word-score-cutoff*)))
      (loop for seg in segments
           while (>= (segment-score seg) cutoff)
           collect seg))))

(defstruct segment-list segments start end (top nil))

(defgeneric get-segment-score (seg)
  (:documentation "Like segment-score but also works for segment-list and synergies")
  (:method ((seg segment))
    (segment-score seg))
  (:method ((seg-list segment-list))
    (let ((seg (car (segment-list-segments seg-list))))
      (if seg (segment-score seg) 0)))
  (:method ((syn synergy))
    (synergy-score syn)))

(defun join-substring-words (str)
  (loop with sticky = (find-sticky-positions str)
       for start from 0 below (length str)
       unless (member start sticky)
       nconcing 
       (loop for end from (1+ start) upto (length str)
            unless (member end sticky)
            nconcing 
            (let ((segments (mapcar 
                             (lambda (word)
                               (gen-score (make-segment :start start :end end :word word)
                                          (= end (length str))))
                             (find-word (subseq str start end)))))
              (when segments
                (list (make-segment-list :segments (cull-segments segments)
                                         :start start :end end)))))))


(defstruct (top-array-item (:conc-name tai-)) score payload)

(defclass top-array ()
  ((array :reader top-array)
   (count :reader item-count :initform 0)
   ))

(defmethod initialize-instance :after ((obj top-array) &key (limit 5))
  (setf (slot-value obj 'array) (make-array limit :initial-element nil)))

(defgeneric register-item (collection score payload)
  (:method ((obj top-array) score payload)
    (with-slots (array count) obj
      (let ((item (make-top-array-item :score score :payload payload))
            (len (length array)))
        (loop for idx from (min count len) downto 0
           for prev-item = (when (> idx 0) (aref array (1- idx)))
           for done = (or (not prev-item) (>= (tai-score prev-item) score))
           when (< idx len) do (setf (aref array idx) (if done item prev-item))
           until done)
        (incf count)))))

(defgeneric get-array (collection)
  (:method ((obj top-array))
    (with-slots (array count) obj
      (if (>= count (length array)) array (subseq array 0 count)))))

(defun find-best-path (segments &key (limit 5))
  ;;assume segments are sorted by (start, end) (as is the result of find-substring-words)
  (let ((top (make-instance 'top-array :limit limit)))
    (register-item top 0 nil)

    (dolist (segment segments)
      (setf (segment-top segment) (make-instance 'top-array :limit limit)))

    (loop for (seg1 . rest) on segments
       when (> (segment-score seg1) 0) do 
         (register-item (segment-top seg1) (segment-score seg1) (list seg1))
         (register-item top (segment-score seg1) (list seg1))
         (loop for seg2 in rest
            for seg2-score = (segment-score seg2)
            when (and (> seg2-score 0) 
                      (>= (segment-start seg2) (segment-end seg1))) do
              (loop for tai across (get-array (segment-top seg1))
                 for accum = (+ (tai-score tai) seg2-score)
                 for path = (cons seg2 (tai-payload tai))
                 do (register-item (segment-top seg2) accum path)
                    (register-item top accum path))))
    (dolist (segment segments)
      (setf (segment-top segment) nil))

    (loop for tai across (get-array top)
         collect (cons (reverse (tai-payload tai)) (tai-score tai)))))

(defstruct synergy description connector score start end)

(defun make-segment-list-from (old-segment-list segments)
  (let ((new-segment-list (copy-segment-list old-segment-list)))
    (setf (segment-list-segments new-segment-list) segments)
    new-segment-list))

(defmacro generic-synergy ((segment-list-left segment-list-right)
                           filter-left filter-right &key description connector score)
  (alexandria:with-gensyms (start end left right)
  `(let ((,start (segment-list-end ,segment-list-left))
         (,end (segment-list-start ,segment-list-right)))
     (when (= ,start ,end)
       (let ((,left (remove-if-not ,filter-left (segment-list-segments ,segment-list-left)))
             (,right (remove-if-not ,filter-right (segment-list-segments ,segment-list-right))))
          (when (and ,left ,right)
            (list (list (make-segment-list-from ,segment-list-right ,right)
                        (make-synergy :start ,start :end ,end
                                      :description ,description
                                      :connector ,connector
                                      :score ,score)
                        (make-segment-list-from ,segment-list-left ,left)))))))))

(defun generic-synergy* (filter-left filter-right &rest keys &key description connector score)
  (declare (ignore description connector score))
  (lambda (segment-list-left segment-list-right)
    (let ((start (segment-list-end segment-list-left))
          (end (segment-list-start segment-list-right)))
      (when (= start end)
        (let ((left (remove-if-not filter-left (segment-list-segments segment-list-left)))
              (right (remove-if-not filter-right (segment-list-segments segment-list-right))))
          (when (and left right)
            (list (list (make-segment-list-from segment-list-right right)
                        (apply #'make-synergy :start start :end end keys)
                        (make-segment-list-from segment-list-left left)))))))))


(defparameter *synergy-list* nil)

(defmacro defsynergy (name (left-var right-var) &body body)
  `(progn
     (defun ,name (,left-var ,right-var)
       ,@body)
     (pushnew ',name *synergy-list*)))

(defsynergy synergy-noun-particle (l r)
  (generic-synergy (l r)
   (lambda (segment)
     (and (typep (segment-word segment) 'kanji-text)
          (intersection '("n" "n-adv" "n-t" "adj-na")
                        (getf (segment-info segment) :posi)
                        :test 'equal)))
   (lambda (segment)
     (and (not (eql (common (segment-word segment)) :null))
          (intersection '("prt" "cop-da")
                        (getf (segment-info segment) :posi)
                        :test 'equal)))
   :description "noun+prt"
   :score 15
   :connector " "))

(defsynergy synergy-te-iru-aru (l r)
  (generic-synergy (l r)
   (lambda (segment)
     (member 3 (getf (segment-info segment) :conj)
             :key (lambda (cdata) (conj-type (conj-data-prop cdata)))))
   (lambda (segment)
     (intersection '(1577980 1296400) ;;  [いる] [ある]
                   (getf (segment-info segment) :seq-set)))
   :description "-te+iru/aru"
   :score 10
   :connector ""))

(defsynergy synergy-suru-verb (l r)
  (generic-synergy (l r)
   (lambda (segment)
     (and (typep (segment-word segment) 'kanji-text)
          (member "vs" (getf (segment-info segment) :posi) :test 'equal)))
   (lambda (segment)
     (member 1157170 (getf (segment-info segment) :seq-set)))
   :description "noun+suru"
   :score 10
   :connector ""))

(defsynergy synergy-o-prefix (l r)
  (generic-synergy (l r)
   (lambda (segment)
     (eql 1270190 (seq (segment-word segment))))
   (lambda (segment)
     (and (typep (segment-word segment) 'kanji-text)
          (member "n" (getf (segment-info segment) :posi) :test 'equal)))
   :description "o+noun"
   :score 10
   :connector ""))

(defun get-synergies (segment-list-left segment-list-right)
  (loop for fn in *synergy-list*
     nconc (funcall fn segment-list-left segment-list-right)))

(defun find-best-path^2 (segment-lists &key (limit 5))
  "same as find-best-path but operates on segment-lists and uses synergies"
  (let ((top (make-instance 'top-array :limit limit)))
    (register-item top 0 nil)

    (dolist (segment-list segment-lists)
      (setf (segment-list-top segment-list) (make-instance 'top-array :limit limit)))

    (loop for (seg1 . rest) on segment-lists
         for score1 = (get-segment-score seg1)
       when (> score1 0) do 
         (register-item (segment-list-top seg1) score1 (list seg1))
         (register-item top score1 (list seg1))
         (loop for seg2 in rest
            for score2 = (get-segment-score seg2)
            when (and (> score2 0)
                      (>= (segment-list-start seg2) (segment-list-end seg1))) do
              ;; here's where it gets really damn hairy
              (loop for tai across (get-array (segment-list-top seg1))
                   for (seg-left . tail) = (tai-payload tai)
                   for score3 = (get-segment-score seg-left)
                   for score-tail = (- (tai-score tai) score3)
                   do (loop for split in (cons (list seg2 seg-left) (get-synergies seg-left seg2))
                           for accum = (+ (reduce #'+ split :key #'get-segment-score) score-tail)
                           for path = (nconc split tail)
                           do (register-item (segment-list-top seg2) accum path)
                              (register-item top accum path)))))

    (dolist (segment segment-lists)
      (setf (segment-list-top segment) nil))

    (loop for tai across (get-array top)
         collect (cons (reverse (tai-payload tai)) (tai-score tai)))))

(defun find-best-path^2-1 (segment-lists &key (limit 5))
  "convert find-best-path^2 results to previous format"
  (let ((result (find-best-path^2 segment-lists :limit limit)))
    (dolist (item result result)
      (setf (car item)
            (mapcan (lambda (obj)
                      (typecase obj
                        (segment-list (list (car (segment-list-segments obj))))))
                    (car item))))))

(defstruct word-info type text kana (score 0) (seq nil))

(defun word-info-from-segment (segment &aux (word (segment-word segment)))
  (make-word-info :type (if (typep word 'kanji-text) :kanji :kana)
                  :text (get-text word)
                  :kana (get-kana word)
                  :seq (seq word)
                  :score (segment-score segment)))

(defun fill-segment-path (str path)
  (flet ((make-substr-gap (start end)
           (let ((substr (subseq str start end)))
             (make-word-info :type :gap :text substr :kana substr))))
    (loop with idx = 0 and result
       for segment in path
       if (> (segment-start segment) idx)
         do (push (make-substr-gap idx (segment-start segment)) result)
       do (push (word-info-from-segment segment) result)
          (setf idx (segment-end segment))
       finally
         (when (< idx (length str))
           (push (make-substr-gap idx (length str)) result))
         (return (nreverse result)))))
  
                      
(defun dict-segment (str &key (limit 5))
  (with-connection *connection*
    (loop for (path . score) in (find-best-path^2-1 (join-substring-words str) :limit limit)
         collect (cons (fill-segment-path str path) score))))

(defun simple-segment (str &key (limit 5))
  (caar (dict-segment str :limit limit)))

(defun get-senses (seq)
  (query (:order-by
          (:select (:select (:concat "[" (:raw "string_agg(pos.text, ',' ORDER BY pos.ord)") "]")
                            :from (:as 'sense-prop 'pos) :where (:and (:= 'pos.sense-id 'sense.id) (:= 'pos.tag "pos")))
                   (:select (:raw "string_agg(gloss.text, '; ' ORDER BY gloss.ord)")
                            :from 'gloss :where (:= 'gloss.sense-id 'sense.id))
                   :from 'sense
                   :where (:= 'sense.seq seq)
                   :group-by 'sense.id)
          'sense.ord)))

(defun get-senses-str (seq)
  (with-output-to-string (s)
    (loop for (pos gloss) in (get-senses seq)
          for i from 1
          for rpos = pos then (if (equal pos "[]") rpos pos)
          when (> i 1) do (terpri s)
          do (format s "~a. ~a ~a" i rpos gloss))))

(defun short-sense-str (seq &key with-pos)
  (query 
   (sql-compile
    `(:limit 
      (:order-by
       (:select (:select (:raw "string_agg(gloss.text, '; ' ORDER BY gloss.ord)")
                         :from gloss :where (:= gloss.sense-id sense.id))
                :from sense 
                ,@(if with-pos
                      `(:inner-join (:as sense-prop pos) :on (:and (:= pos.sense-id sense.id)
                                                                   (:= pos.tag "pos")
                                                                   (:= pos.text ,with-pos))))
                :where (:= 'sense.seq ,seq)
                :group-by 'sense.id)
       'sense.ord)
      1)) :single))

(defun reading-str (kanji kana)
  (if kanji
      (format nil "~a 【~a】" kanji kana)
      kana))

(defun entry-info-short (seq &key with-pos)
  (let ((kanji-text (car (query (:select 'text :from 'kanji-text :where (:and (:= 'seq seq) (:= 'ord 0))) :column)))
        (kana-text (car (query (:select 'text :from 'kana-text :where (:and (:= 'seq seq) (:= 'ord 0))) :column)))
        (sense-str (short-sense-str seq :with-pos with-pos)))
    (with-output-to-string (s)
      (format s "~a : " (reading-str kanji-text kana-text))
      (when sense-str (princ sense-str s)))))

(defun print-conj-info (seq &optional (out *standard-output*))
  (loop for conj in (select-dao 'conjugation (:= 'seq seq))
       do (loop for conj-prop in (select-dao 'conj-prop (:= 'conj-id (id conj)))
             do (format out "~%Conjugation: ~a" (conj-info-short conj-prop)))
       (format out "~%  ~a" (entry-info-short (seq-from conj)))))

(defun word-info-str (word-info)
  (with-connection *connection*
    (with-output-to-string (s)
      (princ (reading-str (case (word-info-type word-info)
                            (:kanji (word-info-text word-info))
                            (t nil))
                          (word-info-kana word-info)) s)
      (terpri s)
      (let ((seq (word-info-seq word-info)))
        (princ (if seq (get-senses-str seq) "???") s)
        (when seq
          (print-conj-info seq s))))))
          
