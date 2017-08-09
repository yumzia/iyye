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

(ns iyye.bios.task)

; FIXME Yumzya: to add task trees
(defn start-task [task]
  (dosync (ref-set (:task-future task) (future (while (not (Thread/interrupted))
                                                       (dorun [((:function task))]))))))

(defn stop-task [task]
  (future-cancel @(:task-future task)))

