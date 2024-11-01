(in-package #:ichiran/conn)

(defparameter *connection* 
  '("jmdict" "ichiran_nameless_fog_5027" "EtbRM0vI6R50GBu" 
    "jmdict.flycast" 
    :port 5432
    :pooled-p t))

(defparameter *connections* '((:old "jmdict" "ichiran_nameless_fog_5027" "EtbRM0vI6R50GBu" "jmdict.flycast" :port 5432)
                             (:test "jmdict" "ichiran_nameless_fog_5027" "zCYTR4Vlui1A6U5" "jmdict.flycast" :port 5432)))

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"/home/you/dump/JMdict_e")

(defparameter *jmdict-data* #p"/root/jmdictdb/jmdictdb/data/")

(in-package #:ichiran/kanji)

(defparameter *kanjidic-path* #P"/home/you/dump/kanjidic2.xml")
