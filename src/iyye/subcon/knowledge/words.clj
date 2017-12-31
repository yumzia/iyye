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

(def relations-words (ref []))
(def types-words (ref []))
(def instances-words (ref []))
(def adjective-words (ref []))
(def action-words (ref []))

(defrecord Iyye_Predicate [AccordingTo When Time Prob])

(defrecord Iyye_Atom [Name Uname predicate])

(defn create-iyye-atom [name according when words-list]
  (let [Time (persistence/current-time-to-string)
        uname (str name (count words-list))
        atom (->Iyye_Atom name uname (->Iyye_Predicate according when Time 1.0))]
    atom))

(defrecord Iyye_Type [atom SubtypeOf Subtypes Predicates])

(defn create-iyye-type [Name AccordingTo When SubtypeOf Subtypes]
  (let [type-atom (create-iyye-atom Name AccordingTo When types-words)
        type (->Iyye_Type type-atom SubtypeOf Subtypes [])]
    type))

(defn load-iyye-atom-from-db [uname]
  )

(defn load-iyye-type-from-db [name]
  )

;(dosync (alter types-words conj type))
;(persistence/write-noun-to-db (into {} type))

(defrecord Iyye_Relation [atom Reason Types Function])

(defn create-iyye-relation [Name AccordingTo Reason When Types Function]
  (let [action-atom (create-iyye-atom Name AccordingTo When @relations-words)
        relation (->Iyye_Relation action-atom Reason Types Function)]
    relation))

(defrecord Iyye_Instance [atom type])

(defn create-iyye-instance [values])

;(dosync (alter relations-words conj relation))
;(persistence/write-action-to-db (conj (into {} (:atom 8action)) (dissoc (into {} action) :atom)))
;
; (defn write-to-db [atom]
;  (persistence/write-knowledge-to-db (conj (into {} (:atom action)) (dissoc (into {} action) :atom))))

;(defn ^{:source "(+ 1 a)"} aaa [a] (+ 1 a))
;(defmacro getsrc [func] `(:source (meta (var ~func))))

(defn iyye_is_type_function [p1 p2]
  (let [p2-type (load-iyye-type-from-db p2)
        newtype (if p2-type p2-type (create-iyye-type p2 :IYE :ALWAYS p1 []))])

  )

(defn iyye_consists_function [p1 p2]
  (let [])
  )

(defn iyye_instance_function [p1 name]
  (let [])
  )

(def iyye_is_type (ref 0))
(def iyye_consists_type (ref 0))
(def iyye_create_instance (ref 0))

(defn action [cmd params IO]
  (let [actions
        (for [action @action-words :when (= (:Name action) cmd)]
          action)]
    (case (count actions)
      0 (ioframes/process-output IO (str "failed to parse: no matching action to " cmd))
      1 (let [action (first actions)]
          (if (compare params (:Types action))
            ((:Function action) params)
            (ioframes/process-output IO (str "failed to parse: types mismatch " cmd ": " action ":" params))))
      (ioframes/process-output IO (str "failed to parse: too many matched action to " cmd ": " actions)))))

(defn- init-builtins-kb []
  (let [t_is (create-iyye-relation "is" :IYE :AXIOM :ALWAYS [] iyye_is_type_function)
        consistsof (create-iyye-relation "consists" :IYE :AXIOM :ALWAYS [] iyye_consists_function)
        instof (create-iyye-relation "instance of" :IYE :AXIOM :ALWAYS [] iyye_instance_function)]
    (dosync (ref-set iyye_is_type t_is))
    (dosync (ref-set iyye_consists_type consistsof))
    (dosync (ref-set iyye_create_instance instof))
    ; (dosync (alter noun-words conj iyye_concept))
    ;(dorun (map #(do ( persistence/write-fact-to-db (into {} %))) @action-words))
    ))

(defn- init-db-kb []
  (let []
    (dosync (alter relations-words #(apply conj %1 %2) (persistence/read-knowledge-from-db "relations" {:When :ALWAYS})))
    (dosync (alter types-words #(apply conj %1 %2) (persistence/read-knowledge-from-db "types" {:When :ALWAYS})))
    ))

(defn load-init-kb []
  (init-builtins-kb)
  (init-db-kb)

  )

(defn init-kb []
  (init-builtins-kb)
  )

; (apply str (rest (str (:When {:When :ALWAYS}))))
