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

(ns iyye.core
  (:require [iyye.iyye-bios :as bios]
            [iyye.resource :as resource]
            [clojure.tools.logging :as log]
    ;[clojure.core.async :as async :refer [thread]]
            ))

; wake sleep state machine

(def load-state? false)

(defn- load-state []
  ;  (dorun (process-wakeup))
  )

(defn- init-state []
   (log/info "first day"))

(def DAY_CHECK_TIME_MILLIS 1000)

(defn main-function []
    (bios/init-bios)

    (if load-state?
      (load-state)
      (init-state))
    (while true
      (let [current-day (resource/create-day)]
        (resource/start-day current-day)

        (while (not (resource/is-day-over? current-day))
          (Thread/sleep DAY_CHECK_TIME_MILLIS))

        (resource/stop-day current-day)

        (let [current-night (resource/create-night current-day)]
          (resource/start-night current-night)
          (while (not (resource/is-night-over? current-night))
            (Thread/sleep DAY_CHECK_TIME_MILLIS))

          (resource/stop-night current-night)))))

(defn -main []
  (-> (Thread. main-function) .start))
  
