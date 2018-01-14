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

(ns iyye.subcon.knowledge.words
  (:require
    [clojure.tools.logging :as log]
    [iyye.bios.ioframes :as ioframes]
    [iyye.bios.persistence :as persistence]
    ;[iyye.subcon.knowledge.relation :as relation]
    ))

(def action-words (ref {}))
(def noun-words (ref {}))
;(def types-words (ref []))
;(def relations-words (ref []))
;(def instances-words (ref []))
;(def adjective-words (ref []))

(defrecord Iyye_ModalPredicate [AccordingTo When Time Prob])
(defrecord Iyye_Atom [Name Uname Builtin])
(defrecord Iyye_Relation [atom Predicate Types Function PredicateFunction Data])
(defrecord Iyye_Type [atom Relations])
(defrecord Iyye_Instance [atom type])

(def atoms-number (ref 0))
(defn inc-atoms! []
  (dosync (ref-set atoms-number (inc @atoms-number)))
  @atoms-number)

(defn create-iyye-atom [name & builtin]
  (let [uname (str name (inc-atoms!))
        b (if (nil? builtin) false (first builtin))
        atom (->Iyye_Atom name uname b)]
    atom))

(defn create-iyye-type [Name & builtin]
  (let [type-atom (create-iyye-atom Name builtin)
        type (->Iyye_Type type-atom [])]
    type))

(defn create-iyye-relation [Name Predicate Types Function PredicateFunction & builtin]
  (let [action-atom (create-iyye-atom Name builtin)
        relation (->Iyye_Relation action-atom Predicate Types Function PredicateFunction [])]
    relation))

(defn create-iyye-instance [values])

(defn load-iyye-atom-from-db [uname])

(defn load-iyye-atoms-from-db [name & [dbname]])

(defn load-iyye-types-from-db [name]
  (load-iyye-atoms-from-db name "types"))

(defn load-iyye-relations-from-db [name]
  (load-iyye-atoms-from-db name "relations"))

(defn save-iyye-type-to-db [type]
  )
;(persistence/write-noun-to-db (into {} type))

(defn get-iyye-atoms [name builtins]
  (let [builtin-types (for [word (vals @builtins) :when (= (:Name (:atom word)) name)] word)]
    (if (empty? builtin-types) :UNKNOWN builtin-types)))

(defn get-iyye-types [name]
  (get-iyye-atoms name noun-words))

(defn get-iyye-relations [name]
  (get-iyye-atoms name action-words))

(defn set-iyye-type! [type]
  (let [uname (:Uname (:atom type))
        builtin (:Builtin (:atom type))]
    (when builtin
      (do
        (dosync (alter noun-words assoc @noun-words {uname type}))
        (save-iyye-type-to-db type)))))

(defn check-params [action params]
  (let [act-params (:Types action)]
    (compare act-params (map #(:Name (:atom %)) params))))  ; FIXME Yumzya context aware compare

(defn apply-relation [relation params]
  (when (check-params relation params)
    ((:Function relation) params)))

(defn apply-query [relation params]
  (when (check-params relation params)
    ((:PredicateFunction relation) params)))

(defn- run-action [cmd params IO func]
  (let [actions-list (get-iyye-relations cmd)
        params-list (map get-iyye-types params)]
    (case (count actions-list)
      0 (ioframes/process-output IO (str "failed to parse: no matching action to " cmd))
      1 (let [action (first actions-list)]
          (when (not (func action params-list))
            (ioframes/process-output IO (str "failed to parse: types mismatch " cmd ": " action ":" params))))
      (let [matching-actions (for [action @actions-list :when (check-params action params-list)] action)]
        (if (empty? matching-actions)
          (ioframes/process-output IO (str "failed to parse: no many matched parameters to " cmd ": " actions-list))
          (func (first matching-actions) params-list)))))) ; FIXME Yumzya first

(defn action [cmd params IO]
  ; (ioframaes/process-output IO (str " scwords: " cmd))
  (if (= \? (last cmd))
    (run-action (subs cmd 0 (dec (count cmd))) params IO apply-query)
    (run-action cmd params IO apply-relation)))
