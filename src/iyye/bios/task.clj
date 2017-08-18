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

(ns iyye.bios.task
  (:require [clojure.zip :as zip]))

;(defrecord Task [name function task-future parent paused])

(def tasks-tree (ref []))

(defn find-task [name]
  (loop [cur (zip/vector-zip @tasks-tree)]
    (if (zip/end? cur)
      nil
      (if (= (:name (zip/node cur)))
        cur
        (recur (zip/next cur))))))

(defn run-on-all-tasks [func]
  (loop [cur (zip/vector-zip @tasks-tree)]
    (if (zip/end? cur)
      nil
      (do
        (if (not (zip/branch? cur))
          (func (zip/node cur)))
        (recur (zip/next cur))))))

(defn add-task [task]
  (if-let [name (:parent task)]
    (if-let [cur (find-task name)]
      (if cur
        (dosync (ref-set tasks-tree
                         (-> cur (zip/append-child task) zip/root))))
      (dosync (alter tasks-tree conj task)))
    (dosync (alter tasks-tree conj task))))

(defn remove-task [task]
  (let [cur (find-task (:name task))]
    (if cur
      (dosync (ref-set tasks-tree (-> cur zip/remove zip/root))))))

; FIXME Yumzya: to add task trees
(defn start-task [task]
  (dosync (ref-set (:task-future task) (future (while (not (Thread/interrupted))
                                                 (dorun [((:function task))])))))
  (add-task task))

(defn run-off-task [task & params]
  (dosync (ref-set (:task-future task) (future (dorun ((:function task) params)))))
  (add-task task))

(defn stop-task [task]
  (future-cancel @(:task-future task))
  (remove-task task))

(defn pause []
  (run-on-all-tasks #(future-cancel @(:task-future %))))

(defn resume []
  (run-on-all-tasks #(ref-set (:task-future %) (future (while (not (Thread/interrupted))
                                                            (dorun [((:function %))]))))))

(defn shutdown []
  "Trivial now. FIXME Yumzya later add saving in-progress tasks"
  (System/exit 0))

