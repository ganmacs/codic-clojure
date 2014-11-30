(ns codic.core-test
  (:use [clojure.test])
  (:require [codic.core :refer :all]
            [clojure-csv.core :as csv]))

(def word-list ["40719,減少" "40720,減少させる" "40722,増加させる" "40723,インクリメントする"
                "40724,増加" "40725,増大" "40726,インクリメント" "40728,必要とする"])
(def dic (csv/parse-csv (clojure.string/join "\n" word-list)))

(deftest test-dictionary-type
  (testing "when keyword is japanese"
    (is (= (dictionary-type "保護する") :naming))
    (is (= (dictionary-type "あいう") :naming)))
  (testing "when keyword is english"
    (is (= (dictionary-type "english") :english))
    (is (= (dictionary-type "protect") :english))))

(deftest test-search-word
  (testing "when found words"
    (is (= (search-word "増加" dic) '(("40722" "増加させる") ("40724" "増加"))))
    (is (= (search-word "必要とする" dic) '(("40728" "必要とする")))))
  (testing "when NOT found words"
    (is (= (search-word "" dic) '()))
    (is (= (search-word "雨だ" dic) '()))))

(deftest tanslate-japanese-to-english-meanings
  (testing "when can tanslate keyword"
    (is (= (codic-translator "増加") '(("増加させる" ("increment")) ("増加" ("increment" "incrementation")) ("増加の" ("incremental"))))))
  (testing "when can NOT  tranlhslate keyword"
    (is (= (codic-translator "") '()) "if empty string get, return empty list")))

(deftest translate-english-to-japanese-meanings
  (testing "when can tanslate keyword"
    (is (= (codic-translator "flip") '(("flip" ("（クルッと）ひっくり返す"))))))
  (testing "when can NOT  tranlhslate keyword"
    (is (= (codic-translator "") '()) "if empty string get, return empty list")))
