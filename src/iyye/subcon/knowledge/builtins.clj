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

(defn iyye_is_type_function [p1-types p2-types]
    (when (and (not(empty? p1-types)) (not(empty? p2-types)))
      (let [p1-type (first p1-types)                        ; FIXME Yumzya first
            p2-type (first p2-types)

            p1-supertypes (assoc @iyye_is_type :Data {:super (:Name (:atom p2-type))})
            p2-subtypes (assoc @iyye_is_type :Data {:sub (:Name (:atom p1-type))})
            new-p1-type (assoc p1-type :Relations p1-supertypes)
            new-p2-type (assoc p2-type :Relations p2-subtypes)]
      (do
        (words/set-iyye-type! new-p1-type)
        (words/set-iyye-type! new-p2-type)))))

(defn iyye_is_type_create_function [p1 p2]
  (let [p1-type (words/create-iyye-type p1)
        p2-type (words/get-iyye-types p2)]
    (when (and p1-type p2-type)
      (let [new-p1-type (assoc p1-type :Relations (conj (:Relations p1-type) ))])
      (do
        ()
        ()
        ()))))

(defn iyye_consists_function [p1 p2]
  (let [])
  )

(defn iyye_instance_function [p1 name]
  (let [])
  )

;(dosync (alter relations-words conj relation))
;(persistence/write-action-to-db (conj (into {} (:atom action)) (dissoc (into {} action) :atom)))
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
        yumzya (words/create-iyye-type "yumzya")]
    (dosync (ref-set iyye_actor actor))
    (dosync (ref-set iyye_ai ai))
    (dosync (ref-set iyye_iyye iyye))
    (dosync (ref-set iyye_human human))
    (dosync (ref-set iyye_yumzya yumzya))
    (dosync (alter words/noun-words conj actor ai iyye human yumzya))))

(defn- init-builtin-relations []
  (let [t_is (words/create-iyye-relation "is" :IYE :AXIOM :ALWAYS ["type" "type"] iyye_is_type_function)
        t_is2 (words/create-iyye-relation "is" :IYE :AXIOM :ALWAYS [:UNKNOWN "type"] iyye_is_type_create_function)
        consistsof (words/create-iyye-relation "consists" :IYE :AXIOM :ALWAYS [] iyye_consists_function)
        instof (words/create-iyye-relation "instance" :IYE :AXIOM :ALWAYS [] iyye_instance_function)]
    (dosync (ref-set iyye_is_type t_is))
    (dosync (ref-set iyye_consists_type consistsof))
    (dosync (ref-set iyye_create_instance instof))
    (dosync (alter words/action-words conj t_is t_is2 consistsof instof))))


(defn- apply-builtin-relations []
  (do
    ((:Function @iyye_is_type) @iyye_ai @iyye_actor)
    ((:Function @iyye_is_type) @iyye_human @iyye_actor)
    ((:Function @iyye_is_type) @iyye_iyye @iyye_ai)
    ((:Function @iyye_is_type) @iyye_yumzya @iyye_human)))

(defn- init-builtins-kb []
  (init-builtin-words)
  (init-builtin-relations)
  (apply-builtin-relations))

;(dorun (map #(do ( persistence/write-fact-to-db (into {} %))) @action-words))
; (apply str (rest (str (:When {:When :ALWAYS}))))

(defn- init-db-kb []
  (let []
    (dosync (alter words/action-words #(apply conj %1 %2) (persistence/read-knowledge-from-db "relations" {:When :ALWAYS})))
    (dosync (alter words/noun-words #(apply conj %1 %2) (persistence/read-knowledge-from-db "types" {:When :ALWAYS})))
    ; others
    ))

(defn load-init-kb []
  (init-builtins-kb)
  (init-db-kb))

(defn init-kb []
  (init-builtins-kb))
