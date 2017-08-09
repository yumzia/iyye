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

(ns iyye.bios.ioframes
  (:require
    [clojure.tools.logging :as log]
    [iyye.bios.persistence :as persistence]
    [iyye.bios.resource :as resource]))

(defrecord InputSources [start-function input-listeners-list])
(defn create-InputSources [start-function input-listeners-list] (InputSources. start-function input-listeners-list))
(defrecord OutputActuators [start-function outputs-list])

(defrecord IO [name start-function inputs-list outputs-list])

(defn create-IO [name start-fumction inputs-list outputs-list] (IO. name start-fumction inputs-list outputs-list))

(defn process-input [IO input]
  (log/info (str "in: " (:name IO) input))
  (dorun (map #(% IO input) (:input-listeners-list (:inputs-list IO))))
  (persistence/write-io-to-db @resource/num-days IO :input input))

(defn process-output [IO output]
  (log/info (str "out:" (:name IO) output))
  (dorun (map #(% IO output) (:outputs-list (:outputs-list IO))))
  (persistence/write-io-to-db @resource/num-days IO :output output))

