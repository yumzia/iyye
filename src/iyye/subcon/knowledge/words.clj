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

(ns iyye.subcon.knowledge.words
  (:require
    [clojure.tools.logging :as log]
    [iyye.bios.ioframes :as ioframes]
    [iyye.bios.persistence :as persistence]
    ;[iyye.subcon.knowledge.relation :as relation]
    ))

(def dbg-IO (ref nil))

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

(defn create-iyye-type-from-db [t]
  "de-serialize t"
  (let [mykey #(keyword (str %1 %2))
        create-types #(clojure.string/split (t (mykey "Relation-Types-List" %1)) #"_")
        create-data #(hash-map (keyword (t (mykey "Relation-Data-List1" %1)))
                       (t (mykey "Relation-Data-List2" %1)))
        relations-list
        (loop [rel-vec []
               num 0]
          (if-not (t (mykey "Relation-name" num))
            rel-vec
            (let [rel-uname (t (mykey "Relation-uname" num))]
              (recur (conj rel-vec
                         (->Iyye_Relation
                           (->Iyye_Atom (t (mykey "Relation-name" num))
                                        rel-uname
                                        false)
                           (->Iyye_ModalPredicate
                             (t (mykey "Relation-AccordingTo" num))
                             (t (mykey "Relation-When" num))
                             (t (mykey "Relation-Time" num))
                             (t (mykey "Relation-Prob" num)))
                           (create-types num)
                           (:Function (@action-words (t (mykey "Relation-uname" num))))
                           (:PredicateFunction (@action-words (t (mykey "Relation-uname" num))))
                           (create-data num)))
                   (inc num)))))]
    (->Iyye_Type (->Iyye_Atom (:Name t) (:Uname t) false) relations-list)))

(defn create-iyye-relation [Name Predicate Types Function PredicateFunction & builtin]
  (let [action-atom (create-iyye-atom Name builtin)
        relation (->Iyye_Relation action-atom Predicate Types Function PredicateFunction [])]
    relation))

(def time-start (persistence/current-time-to-string))
(defn get-supertypes [atype]
  "gets types: self, type, immidiate parents"               ; FIXME Yumzia:0 get all parents
  (if (not (:UNKNOWN atype))
    (let [rels (:Relations atype)]
      (conj (for [supertype rels :when (:super (:Data supertype))]
              {:Name (:super (:Data supertype)) :Predicate (:Predicate supertype)})
              {:Name "type" :Predicate (->Iyye_ModalPredicate :IYE :AXIOM time-start :ALWAYS)}
              {:Name (:Name (:atom atype)) :Predicate (->Iyye_ModalPredicate :IYE :AXIOM time-start :ALWAYS)}))
    {:UNKNOWN (:UNKNOWN atype)}))

(defn create-iyye-instance [values])

(defn load-iyye-atom-from-db [uname])

(defn load-iyye-atoms-from-db [name & [dbname]]
  )

(defn- print-io-str [IO prstr]
  "helper dbg func"
  (future (Thread/sleep 1000) (ioframes/process-output IO (pr-str prstr))))

(defn load-iyye-relations-from-db [name]
  (load-iyye-atoms-from-db name "relations"))

(defn load-iyye-types-from-db [name]
  (let [types (persistence/read-knowledge-from-db "types" {})]
    ))

(defn save-iyye-type-to-db [type]
  (let [types-list-str (fn [types]
          (reduce #(str %1 "_" %2) types))
        rel-key (fn [name ind]
          (keyword (str name ind)))
        get-rel
        (fn [ind rel]
          {(rel-key "Relation-name" ind) (:Name (:atom rel))
             (rel-key "Relation-uname" ind) (:Uname (:atom rel))
            (rel-key "Relation-AccordingTo" ind) (:AccordingTo (:Predicate rel))
            (rel-key "Relation-When" ind) (:When (:Predicate rel))
            (rel-key "Relation-Time" ind) (:Time (:Predicate rel))
             (rel-key "Relation-Prob" ind) (:Prob (:Predicate rel))
             (rel-key "Relation-Types-List" ind) (types-list-str (:Types rel))
           (rel-key "Relation-Data-List1" ind) (types-list-str (map name (keys (:Data rel))))
               (rel-key "Relation-Data-List2" ind) (types-list-str (vals (:Data rel)))
           })]
    (let [write-type (reduce conj {:Uname (:Uname (:atom type))
                                   :Name  (:Name (:atom type))}
                             (map-indexed get-rel (:Relations type)))
          ]
      ; (when @dbg-IO (print-io-str @dbg-IO (pr-str "add list: " type ":::"
      ;                                          (map-indexed get-rel (:Relations type)))))
      (persistence/write-knowledge-to-db "types" write-type))))

(defn save-iyye-builtin-type-to-db [type]

  )
;(persistence/write-noun-to-db (into {} type))


(defn get-iyye-atoms [name builtins]
  "returns a list of found atoms with supplied name"
  (let [builtin-types
        (for [word (vals @builtins) :when (= (:Name (:atom word)) name)] word)]
    (if (empty? builtin-types) (list {:UNKNOWN name}) builtin-types)))

(defn get-iyye-types [name]
  (get-iyye-atoms name noun-words))

(defn get-iyye-relations [name]
  (get-iyye-atoms name action-words))

(defn set-iyye-type! [type]
  (let [uname (:Uname (:atom type))
        builtin (:Builtin (:atom type))]
      (do
        (dosync (alter noun-words #(assoc % uname type)))
        (if (not builtin)
          (save-iyye-type-to-db type)
          (save-iyye-builtin-type-to-db type)))))

(defn check-params [action params]
  (let [act-params (:Types action)]
    (when (= (count params) (count act-params))
      (let [params-types (map #(get-supertypes %) params)
        ;   vec-params (vec (map #(:Name (:atom %)) params))
            pairs (map vector act-params params-types)
            ok (every? true? (for [cur pairs]
                               (if (= :UNKNOWN (first cur))
                                 (if (:UNKNOWN (second cur)) true false)
                                 (some true?
                                       (map #(= (first cur) %) (map :Name (second cur)))))))]
    ok))))  ; FIXME Yumzya context aware compare

(defn apply-relation [relation params]
  (when (check-params relation params)
    ((:Function relation) params)))

(defn apply-query [relation params]
  (when (check-params relation params)
    ((:PredicateFunction relation) params)))

(defn- run-action [cmd params IO func]
  (let [actions-list (get-iyye-relations cmd)
        params-list (apply concat (map get-iyye-types params))]
    (case (count actions-list)
      0 (print-io-str IO (str "failed to parse: no matching action to " cmd))
      1 (let [action (first actions-list)
              result (func action params-list)]
          (if result
            (print-io-str IO result)))
      (let [matching-actions
            (for [action actions-list :when (check-params action params-list)] action)]
        (if (empty? matching-actions)
          (print-io-str IO (pr-str "Cant find " cmd " " params))
          (let [result (func (first matching-actions) params-list)]
            (if result
              (print-io-str IO result)
              (print-io-str IO "False"))))))))


(defn action [cmd params IO]
  (when (not @dbg-IO) (dosync (ref-set dbg-IO IO)))
  (if (= \? (last cmd))
    (run-action (subs cmd 0 (dec (count cmd))) params IO apply-query)
    (run-action cmd params IO apply-relation)))
