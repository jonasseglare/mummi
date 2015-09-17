(defproject mummi "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:plugins [[cider/cider-nrepl "0.8.1"]]}}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [net.mikera/vectorz-clj "0.28.0"]
                 [seesaw "1.4.5"]
                 [com.h2database/h2 "1.4.189"]
                 [org.clojure/java.jdbc "0.4.2"]
                 
                 ;;[com.orientechnologies/orientdb-server "2.1.2"]
                 ;;[com.oracle/javafx-runtime "2.2.0"]
                 [net.mikera/core.matrix "0.38.0"]])
