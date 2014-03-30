(defproject uibuilder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [phronmophobic/penumbra "0.6.6-SNAPSHOT"]
                 [propaganda "0.2.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]]
  :javac-options ["-target" "1.6" "-source" "1.6"]
  :java-cmd "/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home/bin/java"
  :main uibuilder.core)
