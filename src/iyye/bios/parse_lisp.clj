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

(defn make-syntax-tree [text]
  )

(defn test [])
