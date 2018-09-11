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
    [iyye.bios.persistence :as persistence]))
    ;[iyye.subcon.knowledge.relation :as relation]

(def dbg-IO (ref nil))

(def builtin-words (ref {}))

(defrecord Iyye_ModalPredicate [AccordingTo When Time Prob ])
(defrecord Iyye_Name [Name Uname Type Word2vec Tags])
(defrecord Iyye_Type [name Relations toString])
(defrecord Iyye_Verb [name Types Result Function Complexity])
;(defrecord Iyye_Query [verb PredicateFunction])
(defrecord Iyye_Relation [name Types Predicate Function PredicateFunction Data])
(defrecord Iyye_Instance [name type])
(defrecord Iyye_Error [bug toString])

(defn pred-toString [pred] (pr-str ))

(def names-number (ref 0))
(defn inc-names! []
  (dosync (ref-set names-number (inc @names-number)))
  @names-number)

(defn get-types [verb] (if (:Types verb)
                         (:Types verb)
                         (if (:verb verb)
                           ())))

(defn create-iyye-name [name type & builtin]
  (let [uname (str name (inc-names!))
        b (if (nil? builtin) false (first builtin))
        atom (->Iyye_Name name uname type nil {})]
    atom))

(defn create-iyye-type [Name & builtin]
  (let [type-atom (create-iyye-name Name "type" builtin)
        type (->Iyye_Type type-atom (list) pr-str)]
    type))

(defn create-iyye-type-from-db [t]
  "de-serialize t"
  (let [mykey #(keyword (str %1 %2))
        create-types #(clojure.string/split (t (mykey "Relation-Types-List" %1)) #"_")
        create-data #(hash-map (keyword (t (mykey "Relation-Data-List1" %1))) ; Only 2 now
                               (t (mykey "Relation-Data-List2" %1)))
        relations-list
        (loop [rel-vec []
               num 0]
          (if-not (t (mykey "Relation-name" num))
            rel-vec
            (let [rel-uname (t (mykey "Relation-uname" num))]
              (recur (conj rel-vec
                           (->Iyye_Relation
                             (->Iyye_Name (t (mykey "Relation-name" num))
                                          rel-uname
                                          "relation"
                                          nil {})
                             []
                             (->Iyye_ModalPredicate
                               (t (mykey "Relation-AccordingTo" num))
                               (t (mykey "Relation-When" num))
                               (t (mykey "Relation-Time" num))
                               (t (mykey "Relation-Prob" num)))
                             nil
                             ;  (create-types num)
                             ; (:Function (@verb-words (t (mykey "Relation-uname" num))))
                             (:PredicateFunction (@builtin-words (t (mykey "Relation-uname" num))))
                             (create-data num)))
                     (inc num)))))]
    (->Iyye_Type (->Iyye_Name (:Name t) (:Uname t) "type" nil {}) relations-list str)))

(defn create-iyye-verb [Name Types Result Function & builtin]
  (let [action-atom (create-iyye-type Name "verb" builtin)
        relation (->Iyye_Verb action-atom Types Result Function [])]
    relation))

(defn create-iyye-relation [Name Predicate Types Function PredicateFunction & builtin]
  (let [action-atom (create-iyye-name Name "relation" builtin)
        relation (->Iyye_Relation action-atom Types Predicate Function PredicateFunction [])]
    relation))

(def time-start (persistence/local-time-to-string))

(defn get-supertypes [atype]
  "gets types: self, type, immediate parents"               ; FIXME Yumzia:0 get all parents
  (if-not (:UNKNOWN atype)
    (if-not (and (= "type" (:Name (:name atype))) (not= "relation" (:Type (:name atype))))
      (let [rels (:Relations atype)]
       (conj (for [supertype rels :when (:super (:Data supertype))]
                {:Name (:super (:Data supertype)) :Predicate (:Predicate supertype)})
             {:Name (:Type (:name atype)) :Predicate (->Iyye_ModalPredicate :IYE :AXIOM time-start :ALWAYS)}
             {:Name (:Name (:name atype)) :Predicate (->Iyye_ModalPredicate :IYE :AXIOM time-start :ALWAYS)}))
      (list {:Name "type" :Predicate (->Iyye_ModalPredicate :IYE :AXIOM time-start :ALWAYS)}))
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
          {(rel-key "Relation-name" ind)        (:Name (:name rel))
           (rel-key "Relation-uname" ind)       (:Uname (:name rel))
           (rel-key "Relation-AccordingTo" ind) (:AccordingTo (:Predicate rel))
           (rel-key "Relation-When" ind)        (:When (:Predicate rel))
           (rel-key "Relation-Time" ind)        (:Time (:Predicate rel))
           (rel-key "Relation-Prob" ind)        (:Prob (:Predicate rel))
           (rel-key "Relation-Types-List" ind)  (types-list-str (:Types rel))
           (rel-key "Relation-Data-List1" ind)  (types-list-str (map name (keys (:Data rel))))
           (rel-key "Relation-Data-List2" ind)  (types-list-str (vals (:Data rel)))
           })]
    (let [write-type (reduce conj {:Uname (:Uname (:name type))
                                   :Name  (:Name (:name type))}
                             (map-indexed get-rel (:Relations type)))
          ]
      ; (when @dbg-IO (print-io-str @dbg-IO (pr-str "add list: " type ":::"
      ;                                          (map-indexed get-rel (:Relations type)))))
      (persistence/write-knowledge-to-db "types" write-type))))

(defn save-iyye-builtin-type-to-db [type]

  )
;(persistence/write-noun-to-db (into {} type))

(defn get-iyye-atoms [name mypred]
  "returns a list of found atoms with supplied name"
  (let [builtin-types
        (for [word (vals @builtin-words) :when (= (mypred word) name)] word)]
    (if (empty? builtin-types) (list {:UNKNOWN name}) builtin-types)))

(defn get-iyye-types [name]
  (get-iyye-atoms name #(:Name (:name %))))

(defn get-iyye-verbs [name]
  (get-iyye-atoms name #(:Name (:name (:name %)))))

(defn get-iyye-words [name]
  (let [types (get-iyye-types name)
        verbs (get-iyye-verbs name)]
    (if (:UNKNOWN (first types))
      (if (:UNKNOWN (first verbs))
        types
        verbs)
      (if (:UNKNOWN (first verbs))
        types
        (apply conj verbs types)))))

(defn set-iyye-type! [type]
  (let [uname (:Uname (:name type))
        builtin (:Builtin (:name type))]
    (do
      (dosync (alter builtin-words #(assoc % uname type)))
      (if-not builtin
        (save-iyye-type-to-db type)
        (save-iyye-builtin-type-to-db type)))))

(defn check-params [action params]
  "true if params matches action, false other ways"
  (let [act-params (:Types action)
        check-params-count
        (fn [p1 p2]
          (if (= "*" (last p2))
            (>= (count p1) (count p2))
            (= (count p1) (count p2))))]
    (when (check-params-count params act-params)
      (let [params-types (map #(get-supertypes %) (flatten params))
            act-params2 (take (count params-types) act-params)
            ;   vec-params (vec (map #(:Name (:name %)) params))
            pairs (map vector act-params2 params-types)
            check-pair (fn [t check]
                         (case t
                             :UNKNOWN (= :UNKNOWN check)
                             "*" true
                             (some true?
                                   (map #(= t %) (map :Name check)))))]
        (every? true? (for [cur pairs] (check-pair (first cur) (flatten (second cur)))))))))    ; FIXME Yumzya context aware compare/tags

; (defn apply-relation [relation params]
; (when (check-params relation params)
;  ((:Function relation) params)) )

(defn run-action [cmd params IO]
  (let [actions-list (get-iyye-verbs cmd)
        params-list (if (map? (first params))               ; map? is a check for non-string but resolved type
                      params (map get-iyye-words params))]
    (case (count actions-list)
      0 (->Iyye_Error (str "failed to parse: no matching action to " cmd) pr-str)
      1 (let [action (first actions-list)]
          (if (check-params action params-list)
            ((:Function action) params-list)
            (->Iyye_Error (pr-str "Params invalid for " cmd " : " params) pr-str)))
      (let [matching-actions
            (for [action actions-list :when (check-params action params-list)] action)]
        (case (count matching-actions)
          0 (->Iyye_Error (pr-str "Cant find " cmd " " params) pr-str)
          1 ((:Function (first matching-actions)) params-list)
          (->Iyye_Error (pr-str "Too many actions " cmd " " params) pr-str))))))

(defn action [cmd params IO]
  "Params are either text or objects from previous evaluations"
  (when (not @dbg-IO) (dosync (ref-set dbg-IO IO)))         ; set debug
  ; (print-io-str @dbg-IO (str "cmd: " cmd "::" (pr-str params)) )
  (run-action cmd params IO))
