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

(ns iyye.bios.core
  (:require [iyye.bios.iolist :as iolist]
            [iyye.bios.resource :as resource]
            [iyye.bios.persistence :as persistence]
            [iyye.bios.noun-words :as nouns]
            [clojure.tools.logging :as log]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]))

(def DAY_CHECK_TIME_MILLIS 1000)

; wake sleep state machine
(defn main-loop []
  (nouns/add-word "iyye.days" #(list @resource/num-days) nil)
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

(defn- load-state [name]
  (log/info "first day load")
  (persistence/load-current-iyye name)
  (nouns/add-word "iyye.name" #(list name) nil)
  (resource/load-day!)
  (iolist/load-io-state))

(defn- init-state [name]
  (log/info "first day")
  (persistence/set-current-iyye name))

(defn usage [options-summary]
  (->> ["Usage: iyye action/option"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  list    list IYYE"
        "  load     load IYYE"
        "  create   create IYYE"
        ""]
       (str/join \newline)))

(def cli-options
  ;; An option with a required argument
  [["-n" "--name IYYE" "IYYE name"
    :default "new"]
   ;   :validate [#(iyye-exists? %) "Does not exist"]]
   ["-h" "--help"]])

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    ;       (println "opt:" options "args:" arguments "err:" errors "summary:" summary)
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}

      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}

      (and (= 1 (count arguments))
           (#{"create" "list" "load"} (first arguments)))
      {:action (first arguments) :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "list"   (doall (println (persistence/list-iyye) ) (exit 0 ""))
        "create" (if (persistence/iyye-exists? (:name options)) (exit -1 "Already exists") (init-state (:name options)))
        "load"  (if (persistence/iyye-exists? (:name options)) (load-state (:name options)) (exit -1 "Does not exist")))))

  (doall  (iolist/init-io))
  ; Start in a thread so that I can use REPL too
  (-> (Thread. main-loop) .start))
