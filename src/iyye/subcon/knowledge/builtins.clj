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

(ns iyye.subcon.knowledge.builtins (:require
                                     [clojure.tools.logging :as log]
                                     [iyye.bios.persistence :as persistence]
                                     ;[iyye.subcon.knowledge.relation :as relation]
                                     [iyye.subcon.knowledge.words :as words]
                                     ))

; Builtin types
(def iyye_actor (ref 0))
(def iyye_ai (ref 0))
(def iyye_iyye (ref 0))
(def iyye_human (ref 0))
(def iyye_yumzya (ref 0))

; Builtin relations
(def iyye_is_type (ref 0))
(def iyye_consists_type (ref 0))
(def iyye_create_instance (ref 0))

(defn iyye_is_type_function [p1 p2]
  (let [p2-type (words/load-iyye-type-from-db p2)
        newtype (if p2-type p2-type (words/create-iyye-type p2))])
  )

(defn iyye_consists_function [p1 p2]
  (let [])
  )

(defn iyye_instance_function [p1 name]
  (let [])
  )

;(dosync (alter relations-words conj relation))
;(persistence/write-action-to-db (conj (into {} (:atom 8action)) (dissoc (into {} action) :atom)))
;
; (defn write-to-db [atom]
;  (persistence/write-knowledge-to-db (conj (into {} (:atom action)) (dissoc (into {} action) :atom))))

;(defn ^{:source "(+ 1 a)"} aaa [a] (+ 1 a))
;(defmacro getsrc [func] `(:source (meta (var ~func))))

(defn- init-builtin-words []
  (let [actor (words/create-iyye-type "actor")
        ai (words/create-iyye-type "ai")
        iyye (words/create-iyye-type "iyye")
        human (words/create-iyye-type "human")
        yumzya (words/create-iyye-type "yumzya")
        ]
    (dosync (ref-set iyye_actor actor))
    (dosync (ref-set iyye_ai ai))
    (dosync (ref-set iyye_iyye iyye))
    (dosync (ref-set iyye_human human))
    (dosync (ref-set iyye_yumzya yumzya))))

(defn- init-builtin-relations []
  (let [t_is (words/create-iyye-relation "is" :IYE :AXIOM :ALWAYS [] iyye_is_type_function)
        consistsof (words/create-iyye-relation "consists" :IYE :AXIOM :ALWAYS [] iyye_consists_function)
        instof (words/create-iyye-relation "instance of" :IYE :AXIOM :ALWAYS [] iyye_instance_function)
        ]

    (dosync (ref-set iyye_is_type t_is))
    (dosync (ref-set iyye_consists_type consistsof))
    (dosync (ref-set iyye_create_instance instof))))

(defn- apply-builtin-relations []

    )


(defn- init-builtins-kb []
  (init-builtin-words)
  (init-builtin-relations)
  (apply-builtin-relations)

    ; (dosync (alter noun-words conj iyye_concept))
    ;(dorun (map #(do ( persistence/write-fact-to-db (into {} %))) @action-words))
    )

; (apply str (rest (str (:When {:When :ALWAYS}))))


(defn- init-db-kb []
  (let []
    (dosync (alter words/relations-words #(apply conj %1 %2) (persistence/read-knowledge-from-db "relations" {:When :ALWAYS})))
    (dosync (alter words/types-words #(apply conj %1 %2) (persistence/read-knowledge-from-db "types" {:When :ALWAYS})))
    ; others
    ))

(defn load-init-kb []
  (init-builtins-kb)
  (init-db-kb))

(defn init-kb []
  (init-builtins-kb))
