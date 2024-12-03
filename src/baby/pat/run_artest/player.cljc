(ns baby.pat.run-artest.player
  (:import clojure.lang.LineNumberingPushbackReader)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.main :as main]
            [clojure.pprint :as pp]))

;; Learning why the cognitect folks did it this way is exactly why I haven't changed it to be normal looking.
(def ^:private ^:dynamic *exit-items* ::disabled)

;; Wrap a cleanup function in this.
(defn on-exit
  "If running inside a call to repl, queue f to run when REPL exits."
  [f]
  (when-not (= ::disabled *exit-items*)
    (swap! *exit-items* conj f))
  nil)

;; This simplifies calling read-eval-print multimethod, so the functions can be tested
(defn extract-value-leave-context [input]
  (let [read-eval *read-eval*
        value (binding [*read-eval* read-eval] (eval input))
        _ (set! *3 *2)
        _ (set! *2 *1)
        _ (set! *1 value)]
    value))

;; This could be cleaner.
;; Without the do it's simpler, but the caller would decide too much.
(defmulti read-eval-print (fn [variant input] variant))
(defmethod read-eval-print :default
  [_ input]
  (do
    (pp/pprint input)
    (let [value (extract-value-leave-context input)]
      (print "=> ")
      (pp/pprint value)
      (println))))

(defmacro with-read-known
  "Evaluates body with *read-eval* set to a \"known\" value,
   i.e. substituting true for :unknown if necessary."
  [& body]
  `(binding [*read-eval* (if (= :unknown *read-eval*) true *read-eval*)]
     ~@body))

;; Yanked from some clojure.main or .server or whatever to appease babashka.
;; Maybe fix or something.
(defn skip-whitespace
  "Skips whitespace characters on stream s. Returns :line-start, :stream-end,
  or :body to indicate the relative location of the next character on s.
  Interprets comma as whitespace and semicolon as comment to end of line.
  Does not interpret #! as comment to end of line because only one
  character of lookahead is available. The stream must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF to a single
  \\newline."
  [s]
  (loop [c (.read s)]
    (cond
     (= c (int \newline)) :line-start
     (= c -1) :stream-end
     (= c (int \;)) (do (.readLine s) :line-start)
     (or (Character/isWhitespace (char c)) (= c (int \,))) (recur (.read s))
     :else (do (.unread s c) :body))))

(defn skip-if-eol
  "If the next character on stream s is a newline, skips it, otherwise
  leaves the stream untouched. Returns :line-start, :stream-end, or :body
  to indicate the relative location of the next character on s. The stream
  must either be an instance of LineNumberingPushbackReader or duplicate
  its behavior of both supporting .unread and collapsing all of CR, LF, and
  CRLF to a single \\newline."
  [s]
  (let [c (.read s)]
    (cond
     (= c (int \newline)) :line-start
     (= c -1) :stream-end
     :else (do (.unread s c) :body))))

(defn repl-read
  "Enhanced :read hook for repl supporting :repl/quit."
  [request-prompt request-exit]
  (or ({:line-start request-prompt :stream-end request-exit}
        (skip-whitespace *in*))
      (let [input (read {:read-cond :allow} *in*)]
        (skip-if-eol *in*)
        (case input
          :repl/quit request-exit
          input))))

(defn *read-eval-print
  ([] (*read-eval-print :default))
  ([variant]
   (let [request-prompt (Object.)
         request-exit (Object.)         
         input (with-read-known (repl-read request-prompt request-exit))
         result (if (#{request-prompt request-exit} input)
                  input
                  (read-eval-print variant input))]
     {:request-exit request-exit
      :value result})))

(defn repl
  ([] (repl :default))
  ([variant]
   #?(:bb nil
      :clj (let [cl (.getContextClassLoader (Thread/currentThread))]
             (.setContextClassLoader (Thread/currentThread) (clojure.lang.DynamicClassLoader. cl))))
   (main/with-bindings
     (binding [*exit-items* (atom ())]
       (try
         (loop []
           (let [{:keys [value request-exit]} (*read-eval-print variant)]
             (when-not (identical? value request-exit)
               (recur))))
         (finally
           (doseq [item @*exit-items*]
             (item))))))))

#?(:bb (def ^:dynamic *source-path* nil))

(defn- repl-on
  [script]
  (with-open [rdr (LineNumberingPushbackReader. (io/reader script))]
    (binding [*source-path* (str script) *in* rdr]
      (repl))))

(def script-counter (atom 0))

(defn run
  "Run script through transcripting repl in a tearoff namespace."
  [script]
  (if (vector? script)
    (map run script)
    (let [ns (symbol (clojure.string/replace (str "test-player." (str script) "." (swap! script-counter inc)) #"\/" "--"))]
      (prn (list 'comment {:transcript (str script) :namespace ns}))
      (binding [*ns* *ns*]
        (in-ns ns)
        (clojure.core/use 'clojure.core)
        (repl-on script)))))
