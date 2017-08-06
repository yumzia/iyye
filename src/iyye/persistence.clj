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

(ns iyye.persistence
   (:require
     [clojure.tools.logging :as log]
     [extras.mongowrapper :as mongo]))

(defn local-time-to-string []
  (let [ date (java.util.Date.)]  (.format (java.text.SimpleDateFormat. "MM_dd_yyyy_HH_mm_ss") date)))

(def iyye-name (ref ()))
(def iyye-main "iyye_main")

(defn iyye-exists? [name]
  (let [entry (mongo/get-entry iyye-main "iyye_list" {:name name})]
    (if (first entry)  true false)))

(defn add-iyye [name]
  (if (iyye-exists? name)
    false
    (mongo/add-entry iyye-main "iyye_list" {:name name :time (local-time-to-string)} )))

(defn list-iyye []
  (log/info "nothing list")
  (let [entries (mongo/get-entry iyye-main "iyye_list" {})]
    (if entries (map :name entries)  false)))

(defn set-current-iyye [name]
  (dorun
    [(add-iyye name)
    (println "iyye name is : " name)
    (dosync (ref-set iyye-name name))]))

(defn load-current-iyye [name]
  (println "iyye name is : " name)
  (dosync (ref-set iyye-name name)))

(defn record-day [day-num]
  (mongo/add-entry iyye-main (str "days_" @iyye-name) {:number day-num :time (local-time-to-string)}))
(defn record-night [night-num]
  (mongo/add-entry iyye-main (str "nights_" @iyye-name) {:number night-num :time (local-time-to-string)}))

(defn load-days []
  (mongo/get-count iyye-main (str "days_" @iyye-name) {}))

(defn write-io-to-db [days IO dir input]
  (let [doc (str "IO_day_" days)
        dbname (str @iyye-name "_IO_" (:name IO))
        entry {:time (str (.format (java.text.SimpleDateFormat. "MM_dd_yyyy_HH_mm_ss") (java.util.Date.))) dir input}]
    (mongo/add-entry dbname doc entry)))
