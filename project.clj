(defproject weasel "0.4.0-SNAPSHOT"
  :description "websocket REPL environment for ClojureScript"
  :url "http://github.com/tomjakubowski/weasel"
  :license {:name "Unlicense"
            :url "http://unlicense.org/UNLICENSE"
            :distribution :repo}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2311" :scope "provided"]
                 [org.clojure/google-closure-library "0.0-20140226-71326067" :scope "provided"]
                 [http-kit "2.1.18"]]

  :profiles {:dev {:dependencies [[com.cemerick/piggieback "0.1.3"]]}}
  :source-paths ["src/clj" "src/cljs"])
