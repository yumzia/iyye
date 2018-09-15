; Iyye - AI agent
; Copyright (C) 2016-2018  Sasha Yumzya

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
                                     [iyye.subcon.knowledge.words :as words]))

; Builtin types
(def iyye_actor (ref 0))
(def iyye_ai (ref 0))
(def iyye_iyye (ref 0))
(def iyye_human (ref 0))
(def iyye_yumzya (ref 0))

; Builtin verbs and relations
(def iyye_is_type (ref 0))
(def iyye_is?_type (ref 0))
(def iyye_is_type_rel (ref 0))
(def iyye_consists_type (ref 0))
(def iyye_create_instance (ref 0))

(defn iyye_is_type_predicate_function [params]
    (let [[p1-type p2-type] params
          p2-name (:Name (:name (first p2-type)))
          rels1 (:Relations (first p1-type))
          rels2 (for [rel rels1]
                  [rel (and p2-name (= (:super (:Data rel)) p2-name))])
          rels3 (filter #(second %) rels2)]
      (if (> (count rels3) 0)
        (:Predicate (first (first rels3)))
        (words/->Iyye_ModalPredicate :IYE :AXIOM "now" :NEVER)))); FIXME Yumzia modal logic, FIXME 0 recursive

(defn iyye_is_predicate_function [params]
    (let [[relations & rest] params
          rel (first (filter #(= "relation" (:Type (:name %))) relations))]
      (if (= 2 (count rest))
        (if (and (not (:UNKNOWN (first (first rest)))) (not (:UNKNOWN (first (second rest)))))
          (if rel
            ((:PredicateFunction rel) rest)
            (words/->Iyye_Error (pr-str "No relation: " relations) pr-str))
          (words/->Iyye_Error (pr-str "No relation, unknown: " rest) pr-str))
        (words/->Iyye_Error (pr-str "No relation, unrelated, should be 2: " rest) pr-str))))

(defn iyye_add_relation [p1-type p2-type relation]
  (let [is_type relation]
    (let [p1-supertype (assoc is_type :Data {:super (:Name (:name p2-type))})
          p2-subtype (assoc is_type :Data {:sub (:Name (:name p1-type))})
          new-p1-type (assoc p1-type :Relations
                                     (conj (:Relations p1-type) p1-supertype))
          new-p2-type (assoc p2-type :Relations
                                     (conj (:Relations p2-type) p2-subtype))]
      (do
        (words/set-iyye-type! new-p1-type)
        (words/set-iyye-type! new-p2-type)
        true))))

(defn iyye_is_type_function [params]
  (let [[relation & remaining] params]
    ; (case (count relation)
    ;  0 (words/->Iyye_Error (pr-str "No relation: " params) pr-str)
      (if (and (:Predicate relation) (words/check-params relation remaining)) ; is Relation
        ((:Function relation) (cons relation remaining))
        (words/->Iyye_Error (pr-str "Not a relation: " relation) pr-str)))
    ; (let [rels (filter #(and (not (nil? %)) (words/check-params % remaining)) relation)]
    ; (if (> (count rels) 1)
    ;  (words/->Iyye_Error (pr-str "Too many relations: " rels) pr-str)
    ; (if (= (count rels) 1)
    ;  ((:PredicateFunction (first rels)) remaining)
    ;  (words/->Iyye_Error (pr-str "No relation in: " relation) pr-str)
    )

(defn iyye_is_type_function_relation [params]
  (let [[rel type1 type2 & rest] params]
  (if rest
    (words/->Iyye_Error (pr-str "Too many types to relation: " params) pr-str)
    (if type2
      (if (words/check-params rel params)
        (iyye_add_relation type1 type2 (words/builtin-words @iyye_is_type_rel) ))
      (words/->Iyye_Error (pr-str "only one type for relation: " params) pr-str)))))

(defn iyye_is_type_create_function [params]
  (let [[p1-name p2-type] params
        p1-type (words/create-iyye-type (:UNKNOWN p1-name))]
    (iyye_is_type_function [p1-type p2-type])))

(defn iyye_consists_function [params]
  (let [[p1-type p2-type] params]
    (when (and p1-type p2-type)
      (iyye_add_relation p1-type p2-type @iyye_consists_type))))

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

(defn create-iyye-builtin-verb [name types result func]
  (words/create-iyye-verb
    name
    types result func true))

(defn create-iyye-builtin-query [name types func]
  (words/create-iyye-verb
    name
    types "predicate" func true))

(defn create-iyye-builtin-relation [name types func pred-func]
  (words/create-iyye-relation
    name
    (words/->Iyye_ModalPredicate :IYE :AXIOM (persistence/local-time-to-string) :ALWAYS)
    types func pred-func true))

;(defn create-entry [entry] {(:Uname (:name entry)) entry})

(defn- init-builtin-words []
  (let [type (words/create-iyye-type "type" true)
        actor (words/create-iyye-type "actor" true)
        ai (words/create-iyye-type "ai" true)
        iyye (words/create-iyye-type "iyye" true)
        human (words/create-iyye-type "human" true)
        yumzya (words/create-iyye-type "yumzya" true)
        create-entry (fn [entry] {(:Uname (:name entry)) entry})
        uname #(:Uname (:name %))]
     (dosync (ref-set iyye_actor (uname actor)))
     (dosync (ref-set iyye_ai (uname ai)))
     (dosync (ref-set iyye_iyye (uname iyye)))
    (dosync (ref-set iyye_human (uname human)))
    (dosync (ref-set iyye_yumzya (uname yumzya)))
    (dosync (alter words/builtin-words conj (create-entry type) (create-entry actor)
                   (create-entry ai) (create-entry iyye) (create-entry human)
                   (create-entry yumzya)))))

(defn- init-builtin-relations []
  (let [t_is (create-iyye-builtin-relation "type" ["type" "*"] iyye_is_type_function_relation iyye_is_type_predicate_function)
        ;t_is2 (create-iyye-builtin-relation "is" [:UNKNOWN "type"] iyye_is_type_create_function iyye_is_type_predicate_function)
        ;  consistsof (create-iyye-builtin-relation "has" [] iyye_consists_function iyye_is_type_predicate_function)
        ;instof (create-iyye-builtin-relation "instance" ["type"] iyye_instance_function iyye_is_type_predicate_function)
        uname #(:Uname (:name %))
        create-entry (fn [entry] {(:Uname (:name entry)) entry})]
    (dosync (ref-set iyye_is_type_rel (uname t_is)))
    ; (dosync (ref-set iyye_consists_type (uname consistsof)))
    ;  (dosync (ref-set iyye_create_instance (uname instof)))
    (dosync (alter words/builtin-words conj (create-entry t_is)))))

(defn- init-builtin-verbs []
  (let [t_is (create-iyye-builtin-verb "is" ["relation" "type" "*"] "relation" iyye_is_type_function)
        t_is? (create-iyye-builtin-query "is?" ["relation" "type" "*"] iyye_is_predicate_function)
        ;t_is2 (create-iyye-builtin-relation "is" [:UNKNOWN "type"] iyye_is_type_create_function iyye_is_type_predicate_function)
        consistsof (create-iyye-builtin-verb "has" ["type" "*"] "relation" iyye_consists_function)
        ;instof (create-iyye-builtin-relation "instance" ["type"] iyye_instance_function iyye_is_type_predicate_function)
        uname #(:Uname (:name (:name %)))
        create-entry (fn [entry] {(:Uname (:name (:name entry))) entry})]
    (dosync (ref-set iyye_is_type (uname t_is)))
    (dosync (ref-set iyye_is?_type (uname t_is?)))
    (dosync (ref-set iyye_consists_type (uname consistsof)))
    ;   (dosync (ref-set iyye_create_instance (uname instof)))
    (dosync (alter words/builtin-words conj (create-entry t_is) (create-entry t_is?) (create-entry consistsof)))))

(defn apply-builtin-relations []
  (let [get-type #(words/builtin-words %)
        get-function #(:Function (words/builtin-words %))
        f (get-function @iyye_is_type)]
        (when f
          (do
            (f [(get-type @iyye_is_type_rel) (get-type @iyye_ai) (get-type @iyye_actor)])
            (f [(get-type @iyye_is_type_rel) (get-type @iyye_human) (get-type @iyye_actor)])
            (f [(get-type @iyye_is_type_rel) (get-type @iyye_iyye) (get-type @iyye_ai)])
            (f [(get-type @iyye_is_type_rel) (get-type @iyye_yumzya) (get-type @iyye_human)])))))

(defn- init-builtins-kb []
  (init-builtin-words)
  (init-builtin-verbs)
  (init-builtin-relations)
  (apply-builtin-relations)
  )

;(dorun (map #(do ( persistence/write-fact-to-db (into {} %))) @action-words))
; (apply str (rest (str (:When {:When :ALWAYS}))))

(defn- init-db-kb []
  (let [types-loaded (persistence/read-knowledge-from-db "types" {})
        types (map words/create-iyye-type-from-db types-loaded)
        create-entry (fn [entry] {(:Uname (:name entry)) entry})
        a-f (fn [col el] (conj col (create-entry el)))]
    (dosync (alter words/builtin-words #(apply conj %1 %2) (persistence/read-knowledge-from-db "verbs" {}))) ; {:When :ALWAYS}
    (dosync (alter words/builtin-words #(apply conj %1 %2) (persistence/read-knowledge-from-db "relations" {})))
    (when (not-empty types) (dosync (alter words/builtin-words #(apply a-f %1 %2) types)))  ; {:When :ALWAYS}
    ; others
    ))

(defn load-init-kb []
  (init-builtins-kb)
  (init-db-kb))      ; DB only for persistence

(defn init-kb []
  (init-builtins-kb))
