(in-package #:ichiran/conn)

(defparameter *connection* '("jmdict" "default" "endpoint=ep-falling-sunset-a4546ynz;mfRe13gPNzSs" "ep-falling-sunset-a4546ynz-pooler.us-east-1.aws.neon.tech" :use-ssl :require :port 5432))

(defparameter *connections* '((:old "jmdict" "default" "endpoint=ep-falling-sunset-a4546ynz;mfRe13gPNzSs" "ep-falling-sunset-a4546ynz-pooler.us-east-1.aws.neon.tech" :use-ssl :require :port 5432)
                             (:test "jmdict" "default" "endpoint=ep-falling-sunset-a4546ynz;mfRe13gPNzSs" "ep-falling-sunset-a4546ynz-pooler.us-east-1.aws.neon.tech" :use-ssl :require :port 5432)))

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"/home/you/dump/JMdict_e")

(defparameter *jmdict-data* #p"/root/jmdictdb/jmdictdb/data/")

(in-package #:ichiran/kanji)

(defparameter *kanjidic-path* #P"/home/you/dump/kanjidic2.xml")
