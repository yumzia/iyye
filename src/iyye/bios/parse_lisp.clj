; Iyye - AI agent
; Copyright (C) 2016-2017  Sasha Yumzya

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as
; published by the Free Software Foundation, either version 3 of the
; License, or (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.

; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns iyye.bios.parse-lisp
  (:require [clojure.string :as str]))

(defn partition
  "Splits the string into a lazy sequence of substrings, alternating
  between substrings that match the patthern and the substrings
  between the matches.  The sequence always starts with the substring
  before the first match, or an empty string if the beginning of the
  string matches.

  For example: (partition #\"[a-z]+\" \"abc123def\")
  returns: (\"\" \"abc\" \"123\" \"def\")"
  [re s]
  (let [m (re-matcher re s)]
    ((fn step [prevend]
       (lazy-seq
         (if (.find m)
           (cons (.subSequence s prevend (.start m))
                 (cons (re-groups m)
                       (step (+ (.start m) (count (.group m))))))
           (when (< prevend (.length s))
             (list (.subSequence s prevend (.length s)))))))
      0)))

(defn tokenize [s]
  (filter #(not (= "" %))
          (map #(str/trim %)
               (partition #"\(| |\)|\"[^\"]*\"" s))))

(defn balanced? [tokens]
  (let [n (count tokens)]
    (loop [i 0, current 0]
      (if (= i n)
        (= current 0)
        (if (or (< current 0) (and (= current 0) (not= i 0)))
          false
          (cond
            (= (nth tokens i) "(") (recur (inc i) (inc current))
            (= (nth tokens i) ")") (recur (inc i) (dec current))
            :default (recur (inc i) current)))))))

(defn valid-text? [tokens]
  (and (= "(" (first tokens))
       (= ")" (last tokens))
       (balanced? tokens)))

; Easy recursive parsing, text has been validated already so no error checks
(defn build-tree2 [tokens]
  (let [len (count tokens)]
    (loop [i (dec len) ret-list '()]
      (let [current (nth tokens i)
            find-j (fn [j]
                     (loop [k j count 1]
                       (let [current (nth tokens k)]
                         (cond
                           (= ")" current) (recur (dec k) (inc count))
                           (= "(" current) (if (= 0 (dec count))
                                             k
                                             (recur (dec k) (dec count)))
                           :default (recur (dec k) count)))))]
        (if (= 0 i)
          ret-list
          (if (= current ")")
            (let [j (find-j (dec i))]
              (recur (dec (dec j)) (cons
                                     {:syntax (apply str (butlast (nth tokens (dec j))))
                                      :subtree (build-tree2 (subvec tokens j i))} ret-list)))
            (recur (dec i) (cons current ret-list))))))))

(defn count-tokens [node]
  (loop [i 0 counter 2]
    (if (>= i (count node))
      counter
      (let [elem (nth node i)]
        (if (not (seq? elem))
          (recur (inc i) (inc counter))
          (recur (inc i) (+ counter (count-tokens elem))))))))

(defn build-tree [tokens]
  (loop [i 0 node ()]
    (if (>= i (count tokens))
      node
      (let [token (nth tokens i)]
        (cond (= "(" token)
              (let [sub-node (build-tree (drop (inc i) tokens))]
                (recur (+ i (count-tokens sub-node))
                       (concat node (list sub-node))))
              (= ")" token)
              node
              :default
              (recur (inc i) (concat node (list token)))))))
  )


(defn get-syntax-tree [text]
  (let [tokens (tokenize text)]
    (when (valid-text? tokens) ; Text syntax is valid
      (let [tree (build-tree tokens)]
        (when (= 1 (count tree))
          (first tree))))))
