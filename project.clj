(defproject iyye "0.0.1-SNAPSHOT"
  :description "ai agent experiment"
  :url "http://github.com/yumzia/iyye"
  :license {:name "Gnu Affero General Public License"
            :url "http://www.gnu.org/licenses/agpl.html"}
  :plugins [[lein-cljsbuild "0.2.9"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [ring "1.1.6"]
                 [hiccup "1.0.1"]
                 [clj-json "0.5.1"]
                 ; webbit / websocket
                 [org.webbitserver/webbit "0.4.14"]

                 [aleph "0.3.0-alpha2"]
                 [compojure "1.1.1"]
                 [lein-swank "1.4.4"]
                 [com.novemberain/monger "3.1.0"]
                 [org.clojure/tools.cli "0.3.5"]]
  :source-paths ["src/iyye" "src/extras" "src"]
  :main iyye.core)
