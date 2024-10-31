(in-package #:ichiran/conn)

(defparameter *connection* '("jmdict" "postgres.bayzeytbybzmgfqykhhc" "zitgom-Huqpyv-6wuqso" "aws-0-us-east-1.pooler.supabase.com" :use-ssl :require :port 6543))

(defparameter *connections* '((:old "jmdict" "postgres.bayzeytbybzmgfqykhhc" "zitgom-Huqpyv-6wuqso" "aws-0-us-east-1.pooler.supabase.com" :use-ssl :require :port 6543)
                             (:test "jmdict" "postgres.bayzeytbybzmgfqykhhc" "zitgom-Huqpyv-6wuqso" "aws-0-us-east-1.pooler.supabase.com" :use-ssl :require :port 6543)))

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"/home/you/dump/JMdict_e")

(defparameter *jmdict-data* #p"/root/jmdictdb/jmdictdb/data/")

(in-package #:ichiran/kanji)

(defparameter *kanjidic-path* #P"/home/you/dump/kanjidic2.xml")
