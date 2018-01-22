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

(ns iyye.bios.process-lisp
   (:require [clojure.string :as str]
             [iyye.bios.parse-lisp :as parse]
             [iyye.bios.ioframes :as ioframes]
             [iyye.bios.words :as words]
             [iyye.subcon.knowledge.words :as scwords]))

(defn process-control [tree input IO]
  (let [cmd (first tree)
        params (for [cur (rest tree)] (if (seq? cur) (process-control cur (str cur) IO) cur))]
    ;    (println "processing" cmd)
    (if cmd
      (dorun (words/action cmd params IO))
      (ioframes/process-output IO (str "failed to parse: " input)))))

(defn process-lispy-control [tree input IO]
  (let [cmd (first tree)
        params (for [cur (rest tree)]
                 (if (seq? cur) (process-lispy-control cur (str cur) IO) cur))]
    (if cmd
      (dorun (scwords/action cmd params IO))
      (future (Thread/sleep 500) (ioframes/process-output IO (str "failed to parse: " input))))))

(defn process-lispy-ctrl-input [IO input]
  (let [tree (parse/get-syntax-tree input)]
    (future (Thread/sleep 500)
            (process-control tree input IO))))

(defn process-lispy-input [IO input]
  (if (= \# (first input))
    (process-lispy-ctrl-input IO (subs input 1))
    ;(future (Thread/sleep 1000) (ioframes/process-output IO "parsing lispy sentences not implemented yet"))
    (let [tree (parse/get-syntax-tree input)]
      (future (process-lispy-control tree input IO)))))

(defn process-input [IO input]
   (if (= input "What is answer to life, the universe and everything?")
     (future (Thread/sleep 1000) ( ioframes/process-output IO "42" ))
     (if (= \# (first input))
       (process-lispy-input IO (subs input 1))
       (future (Thread/sleep 1000) (ioframes/process-output IO "parsing sentences not implemented" )))))
