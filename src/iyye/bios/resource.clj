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

(ns iyye.bios.resource
  (:require
    [iyye.bios.task :refer [start-task stop-task]]
    [iyye.bios.persistence :as persistence]
    [clojure.tools.logging :as log]))

(defprotocol Resource
  (get-current-percentage [this]))

; Simple time, e.g. for 30 minutes "day"
(def thirty-minute-day 1000000)

(defrecord TimeResource [start-time end-time])

(extend-type TimeResource
  Resource
  (get-current-percentage [this]
    (- 100 (float (* 100 (/ (- (System/currentTimeMillis) (:start-time this))
                            (- (:end-time this) (:start-time this))))))))

(def ^:private all-day-resources (ref ()))
(def ^:private all-night-resources (ref ()))

(defn register-day-resource [resource]
  (dosync (alter all-day-resources conj resource)))

(defn register-night-resource [resource]
  (dosync (alter all-night-resources conj resource)))

(defn are-resources-exhausted? [day]
  "Returns true if there is a Resource that has left with less than Threshold capacity"
  (let [start-time (:day-start day)
        threshold (:threshold day)
        time-since-start (- (System/currentTimeMillis) start-time)
        day-time (:day-time day)
        time-ratio (/ time-since-start day-time)]
    (if (> (count @all-day-resources) 0)
      (< (* (reduce + (map get-current-percentage @all-day-resources)) time-ratio) threshold)
      (< (- 1 time-ratio) threshold))))

(defn are-resources-ok? [day]
  "Returns if day resources are ok"
  (if (> (count @all-day-resources) 0)
    (> (reduce + (map get-current-percentage @all-day-resources)) (- 1 (:threshold day)))
    true))

(defn are-resources-replenished? [night]
  "Returns true if all Resources have more than Threshold capacity"
  (let [threshold (:threshold night)
        day-resources-ok (are-resources-ok? (:day night))]
    (if (> (count @all-night-resources) 0)
      (< (first (sort (map get-current-percentage @all-night-resources))) threshold)
      day-resources-ok)))

(def num-days (ref 0))

(defrecord DayRoutine [name day-start day-time threshold function task-future parent])
(defrecord NightRoutine [name day night-start night-time threshold function task-future parent])

(def awake-or-sleep (ref false))
(defn is-awake? [] @awake-or-sleep)
(defn is-asleep? []
  (not is-awake?))

(def DAY_TIME thirty-minute-day)
(def DAY_THRESHOLD 0.1)

(defn create-day []
  (DayRoutine. "iyye.bios.day-task" (System/currentTimeMillis) DAY_TIME DAY_THRESHOLD #(Thread/sleep 1000) (ref 0) "iyye.bios.main" ))

(defn create-night [day]
  (NightRoutine. "iyye.bios.night-task" day (System/currentTimeMillis) DAY_TIME DAY_THRESHOLD #(Thread/sleep 1000) (ref 0) "iyye.bios.main" ))

(defn start-day [day]
  (doall
    [(start-task day)
     (dosync ref-set (:day-start day) (System/currentTimeMillis))
     (dosync ref-set awake-or-sleep true)
     (dosync (alter num-days inc))
     (persistence/record-day @num-days)
     (log/info "next day: " @num-days)]))

(defn is-day-over? [day] (are-resources-exhausted? day))

(defn is-night-over? [night] (are-resources-replenished? night))

(defn start-night [night]
  (doall
    [(start-task night)
     (persistence/record-night @num-days)
     (dosync ref-set (:night-start night) (System/currentTimeMillis))]))

(defn stop-day [day]
  (dorun
    [(stop-task day)
     (dosync ref-set awake-or-sleep false)]))

(defn stop-night [night]
  (dorun
    [(stop-task night)]))

(defn load-day! []
  (dosync (ref-set num-days (persistence/load-days))))
