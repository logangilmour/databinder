(defproject databinder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [potemkin "0.2.2-SNAPSHOT"]
                 [lamina "0.5.0-beta15" :exculusions [potemkin]]
                 [aleph "0.3.0-beta15" :exclusions [lamina potemkin]]
                 [clj-http "0.7.0"]
                 [org.apache.jena/apache-jena "2.10.0" :extension "pom"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.2"]
                 [edu.ucdenver.ccp/kr-jena-core "1.4.5"]
                 [org.clojure/tools.logging "0.2.6"]
                 [log4j/log4j "1.2.16" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]
                 [org.clojure/tools.logging "0.2.3"]
                 [org.mozilla/rhino "1.7R4"]]
  :repositories { "apache-releases" "http://repository.apache.org/content/repositories/releases/"}
  :main databinder.core)
