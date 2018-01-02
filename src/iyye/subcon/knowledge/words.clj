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

(def action-words (ref []))
(def noun-words (ref []))
(def types-words (ref []))
(def relations-words (ref []))
(def instances-words (ref []))
(def adjective-words (ref []))

(defrecord Iyye_ModalPredicate [AccordingTo When Time Prob])
(defrecord Iyye_Atom [Name Uname])
(defrecord Iyye_Relation [atom Predicate Types Function])
(defrecord Iyye_Type [atom SubtypeOf Subtypes])
(defrecord Iyye_Instance [atom type])

(defn create-iyye-atom [name words-list]
  (let [Time (persistence/current-time-to-string)
        uname (str name (count words-list))
        atom (->Iyye_Atom name uname)]
    atom))

(defn create-iyye-type [Name]
  (let [type-atom (create-iyye-atom Name types-words)
        type (->Iyye_Type type-atom [] [])]
    type))

(defn create-iyye-relation [Name AccordingTo Reason When Types Function]
  (let [action-atom (create-iyye-atom Name @relations-words)
        relation (->Iyye_Relation action-atom Reason Types Function)]
    relation))

(defn create-iyye-instance [values])

(defn load-iyye-atom-from-db [uname]
  )

(defn load-iyye-type-from-db [name]
  )

(defn get-iyye-type [name]
  (if ()
    ()
    (load-iyye-type-from-db name))
  )

;(dosync (alter types-words conj type))
;(persistence/write-noun-to-db (into {} type))


(defn action [cmd params IO]
  (let [actions
        (for [action @action-words :when (= (:Name action) cmd)] action)
        params-list (map #(for [word @noun-words :when (= (:Name word) %)] word) params)]
    (case (count actions)
      0 (ioframes/process-output IO (str "failed to parse: no matching action to " cmd))
      1 (let [action (first actions)]
          (if (compare (map :Type params-list) (:Types action))
            ((:Function action) params)
            (ioframes/process-output IO (str "failed to parse: types mismatch " cmd ": " action ":" params))))
      (ioframes/process-output IO (str "failed to parse: too many matched action to " cmd ": " actions)))))
