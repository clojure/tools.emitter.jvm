(defproject org.clojure/tools.emitter.jvm "0.1.0-SNAPSHOT"
  :description "A JVM bytecode generator for ASTs compatible with tools.analyzer(.jvm)."
  :url "https://github.com/clojure/tools.emitter.jvm"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.analyzer.jvm "0.3.0"]
                 [org.clojure/tools.reader "0.8.5"]
                 [org.ow2.asm/asm-all "4.2"]])
