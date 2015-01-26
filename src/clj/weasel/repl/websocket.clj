(ns weasel.repl.websocket
  (:refer-clojure :exclude [loaded-libs])
  (:require [cljs.repl]
            [cljs.closure :as cljsc]
            [cljs.compiler :as cmp]
            [cljs.js-deps :as js-deps]
            [cljs.env :as env]
            [clojure.set :as set]
            [clojure.string]
            [weasel.repl.server :as server]))

(declare send-for-eval!)
(declare send-for-eval-file!)

(def loaded-libs (atom #{}))
(def preloaded-libs (atom #{}))

(def ^:private repl-out
  "stores the value of *out* when the server is started"
  (atom nil))

(def ^:private client-response
  "stores a promise fulfilled by a client's eval response"
  (atom nil))

(declare
  websocket-setup-env
  websocket-eval
  websocket-eval-via-file
  load-javascript
  load-javascript-via-file
  websocket-tear-down-env
  transitive-deps)

(defrecord WebsocketEnv []
  cljs.repl/IJavaScriptEnv
  (-setup [this _] (websocket-setup-env this))
  (-evaluate [_ _ _ js] (websocket-eval js))
  (-load [this ns url] (load-javascript this ns url))
  (-tear-down [_] (websocket-tear-down-env)))

(defrecord WebsocketEnvViaFile []
  cljs.repl/IJavaScriptEnv
  (-setup [this] (websocket-setup-env this))
  (-evaluate [_ _ _ js] (websocket-eval-via-file js))
  (-load [this ns url] (load-javascript-via-file this ns url))
  (-tear-down [_] (websocket-tear-down-env))
  )

(defn repl-env
  "Returns a JS environment to pass to repl or piggieback"
  [& {:as opts}]
  (let [compiler-env (env/default-compiler-env opts)
        opts (merge (if (:eval-via-file opts)
                      (WebsocketEnvViaFile.)
                      (WebsocketEnv.))
                    {::env/compiler  compiler-env
                     :ip             "127.0.0.1"
                     :port           9001
                     :preloaded-libs []
                     :src            "src/"}
                    opts)]
    (swap! compiler-env assoc :js-dependency-index (js-deps/js-dependency-index opts))
    (env/with-compiler-env (::env/compiler opts)
                           (reset! preloaded-libs
                                   (set/union
                                     (transitive-deps ["weasel.repl"] {:output-dir "target/weasel/repl"})
                                     (into #{} (map str (:preloaded-libs opts)))))
                           (reset! loaded-libs @preloaded-libs))
    opts))

(defmulti ^:private process-message (fn [_ msg] (:op msg)))

(defmethod process-message
  :result
  [_ message]
  (let [result (:value message)]
    (when-not (nil? @client-response)
      (deliver @client-response result))))

(defmethod process-message
  :print
  [_ message]
  (let [string (:value message)]
    (binding [*out* (or @repl-out *out*)]
      (print (read-string string)))))

(defmethod process-message
  :ready
  [renv _]
  (reset! loaded-libs @preloaded-libs)
  (env/with-compiler-env (::env/compiler renv)
    (send-for-eval! (cljsc/compile-form-seq '[(ns cljs.user)]))))

(defn- websocket-setup-env
  [this]
  (reset! repl-out *out*)
  (require 'cljs.repl.reflect)
  (cljs.repl/analyze-source (:src this))
  (server/start
    (fn [data] (process-message this (read-string data)))
    :ip (:ip this)
    :port (:port this))
  (let [{:keys [ip port]} this]
    (println (str "<< started Weasel server on ws://" ip ":" port " >>"))))

(defn- websocket-tear-down-env
  []
  (reset! repl-out nil)
  (reset! loaded-libs #{})
  (server/stop)
  (println "<< stopped server >>"))

(defn- websocket-eval
  [js]
  (reset! client-response (promise))
  (send-for-eval! js)
  (let [ret @@client-response]
    (reset! client-response nil)
    ret))


(defn- eval-expression?
  [js]
  (-> (re-matches #"(?is)^\s*cljs\.core\.pr_str\.call.*" js)
      boolean)
  )

(defn ->temp-file
  [js]
  (let [file (java.io.File/createTempFile (str "weasel_js_"
                                               (System/currentTimeMillis)
                                               )
                                          ".js")
        path (.getAbsolutePath file)
        expression? (eval-expression? js)]

    (with-open [w (clojure.java.io/writer path)]
      (if expression?
        (.write w "try {window.__weasel_tmp_err=null; window.__weasel_tmp=")
        (.write w "try {window.__weasel_tmp_err=null;")
        )
      (.write w js)
      (.write w "} catch(e) { console.error(e); window.__weasel_tmp_err=e;}")
      (.close w)
      )
    path
    )
  )

(defn- websocket-eval-via-file
  [js]
  (let [fpath (->temp-file js)]
    (reset! client-response (promise))
    (send-for-eval-file! fpath)
    (let [ret @@client-response]
      (reset! client-response nil)
      ret)
    )
  )

(defn- load-javascript
  [_ nses url]
  (when-let [not-loaded (seq (remove @loaded-libs nses))]
    (websocket-eval (slurp url))
    (swap! loaded-libs #(apply conj % not-loaded))))


(defn- load-javascript-via-file
  [_ nses url]
  (when-let [not-loaded (seq (remove @loaded-libs nses))]
    (websocket-eval-via-file (slurp url))
    (swap! loaded-libs #(apply conj % not-loaded))))


(defn- transitive-deps
  "Returns a flattened set of all transitive namespaces required and
  provided by the given sequence of namespaces"
  [nses opts]
  (let [collect-deps #(flatten (mapcat (juxt :provides :requires) %))
        cljs-deps (->> nses (cljsc/cljs-dependencies opts) collect-deps)
        js-deps (->> cljs-deps (cljsc/js-dependencies opts) collect-deps)]
    (disj (into #{} (concat js-deps cljs-deps)) nil)))

(defn- send-for-eval! [js]
  (server/send! (pr-str {:op :eval-js, :code js})))
(defn- send-for-eval-file! [fpath]
  (server/send! (pr-str {:op :eval-js-file, :file fpath})))

(comment
  (let [user-env '{:ns nil :locals {}}
        cenv (atom {})]
    (env/with-compiler-env cenv
      (pprint (mapcat (juxt :provides :requires)
                 (cljsc/cljs-dependencies {} ["weasel.repl"])))))
  )
