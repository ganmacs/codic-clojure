(ns codic.core
  (:gen-class)
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def codic-file-prefix "./resources/codic/")
(def naming-entry (str codic-file-prefix "naming-entry.csv"))
(def naming-translation (str codic-file-prefix "naming-translation.csv"))
(def english-entry (str codic-file-prefix "english-entry.csv"))
(def english-translation (str codic-file-prefix "english-translation.csv"))

(defn dictionary-type [word]
  (let [en? (re-find #"[a-zA-Z_]+" word)]
    (if en? :english :naming)))

(defn search-word [keyword dic]
  (if (= keyword "")
    '()
    (let [reg (re-pattern (str "(?i).*" keyword ".*")) ; case-insentive
          exist-word? (fn [[_ v]] (re-find reg v))]
      (filter exist-word? dic))))

(defn codic-translator
  "search codic meaning by keyword"
  [keyword]
  (let [data-type     (dictionary-type keyword)
        load-csv-file (fn [filename] (csv/parse-csv (slurp filename)))
        [entry-data translate-data] (case data-type
                                      :english [english-entry english-translation]
                                      :naming  [naming-entry naming-translation])
        meaning        (case data-type
                         :english (fn [[id _ _ _ mean _]] (list id mean))
                         :naming  (fn [[id _ _ mean _]] (list id mean)))
        id-word        (fn [[id word _]] (list id word))
        val            (fn [[_ v]] v)
        naming-lists   (group-by first (map meaning (load-csv-file translate-data)))
        searched-words (map id-word (search-word keyword (load-csv-file entry-data)))
        translate      (fn [[id word]] (list word (map val (naming-lists id))))]
    (map translate searched-words)))

(defn printer
  "format codic-tranlator's ret"
  [coll]
  (let [index-form (fn [w] (format "[%s]\n" w))
        item-form (fn [w] (format "* %s\n" w))
        cell-form (fn [[word meanings]]
                    (str (index-form word)
                         (str/join (map item-form meanings))))
        contains (str/join "\n" (map cell-form coll))]
    (println contains)))

(defn -main
  [& args]
  (printer (codic-translator (first args))))
