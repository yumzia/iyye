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

(ns iyye.bios.words
   (:require [clojure.string :as str]
             [iyye.bios.ioframes :as ioframes]
             [iyye.bios.task :as task]
             [iyye.bios.noun-words :as nouns]))

(defrecord action-word [word function async-function-task])

(defn list-func [IO arg]
  (let [noun-names (pr-str (map :word @nouns/noun-words-list))]
 (ioframes/process-output IO (str "list of nouns: " noun-names))))

(defn getter-func [IO arg]
  (let [name (first arg)]
    (let [matches
          (for [noun @nouns/noun-words-list :when (= (:word noun) name)]
            noun)]
      (case (count matches)
        0 (ioframes/process-output IO (str "failed to parse: no matching getter for " name))
        1 (let [action (first matches)
                result ((:getter-function action))]
          (ioframes/process-output IO (str "The value of " name " is " result)))
        (ioframes/process-output IO (str "failed to parse: too many matched getters to " name))))))

(defn setter-func [IO arg]
  (if (not= 2 (count arg))
    (ioframes/process-output IO (str "Can't set, need 2 arguments")))
  (let [name (first arg)
        value (second arg)]
    (let [matches
          (for [noun @nouns/noun-words-list :when (= (:word noun) name)]
            noun)]
      (case (count matches)
        0 (ioframes/process-output IO (str "failed to parse: no matching setter for " name))
        1 (let [action (first matches)
                func (:setter-function action)]
            (if func
              (ioframes/process-output IO (str "Set " name " to " value " " (func value)))
              (ioframes/process-output IO (str "Can't set " name " to " value))))
        (ioframes/process-output IO (str "failed to parse: too many matched setters to " name))))))

(defn shutdown-func [IO arg]
  (ioframes/process-output IO (str "Shutting down!"))
  (task/shutdown))

(defn pause-func [IO arg]
  (task/pause)
  (ioframes/process-output IO (str "Paused")))

(defn resume-func [IO arg]
  (task/resume)
  (ioframes/process-output IO (str "Resumed")))

(defn help-func [IO arg]
  (ioframes/process-output IO (str "Available actions: list, get, set, shutdown, pause, resume, help")))

(def list-action (action-word. "list" list-func nil))
(def get-action (action-word. "get" getter-func nil))
(def set-action (action-word. "set" setter-func nil))
(def shutdown-action (action-word. "shutdown" shutdown-func nil))
(def pause-action (action-word. "pause" pause-func nil))
(def resume-action (action-word. "resume" resume-func nil))
(def help-action (action-word. "help" help-func nil))

; FIXME to add runtime configuring
(def action-words (ref (list get-action set-action shutdown-action pause-action resume-action help-action list-action)))

(defn action [cmd params IO]
  (let [actions
        (for [action @action-words :when (= (:word action) cmd)]
            action)]
    (case (count actions)
      0 (ioframes/process-output IO (str "failed to parse: no matching action to " cmd))
      1 (let [action (first actions)]
          (if (:async-function-task action)
            (task/run-off-task (:async-function-task action) params)
            ((:function action) IO params)))
      (ioframes/process-output IO (str "failed to parse: too many matched action to " cmd ": " actions)))))

