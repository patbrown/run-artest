(ns baby.pat.run-artest
  (:require [babashka.fs :as fs]
            [baby.pat.run-artest.player :as player]
            [baby.pat.vt :as vt]
            [baby.pat.tpl :as TPL]
            [clojure.spec.alpha :as s]            
            [orchestra.core :refer [defn-spec]]
            [selmer.parser]
            [tick.core]))

(def default-templates-path (or (System/getenv "RUN_ARTEST_TEMPLATES")
                                           "resources/assets/templates/"))

(def <-tpl (partial TPL/<-tpl default-templates-path))
(def <-tpl-vars (partial TPL/<-tpl-vars default-templates-path))

(def default-scripts-path (or (System/getenv "RUN_ARTEST_SCRIPTS")
                              "resources/assets/scripts/"))

(defmacro check!
  "Checks v (defaults to *1) against spec, throwing on failure. Returns nil."
  ([spec]
   `(check! ~spec *1))
  ([spec v]
   `(let [v# ~v]
      (when-not (s/valid? ~spec v#)
        (let [ed# (clojure.spec.alpha/explain ~spec v#)
              err# (ex-info "FAIL: " (or ed# {:spec ~spec :value v#}))]
          (throw err#))))))

(defn-spec get-script-file-path ::vt/str
  ([script ::vt/kw-or-str] (get-script-file-path default-scripts-path script))
  ([prefix ::vt/str script ::vt/kw-or-str]
   (cond
     (qualified-keyword? script) (str prefix (namespace script) "/" (name script))
     (keyword? script) (if (fs/exists? (str prefix (name script)))
                         (mapv str (fs/list-dir (str prefix (name script))))
                         (str prefix (name script)))
     (string? script) script)))

(defn-spec QT ::vt/any
  ([scripts ::vt/coll-or-str] (QT default-scripts-path scripts))
  ([prefix ::vt/str scripts ::vt/coll-or-str]
   (let [scripts (if-not (coll? scripts) [scripts] scripts)
         files (mapv (fn [script]
                       (get-script-file-path prefix script)) scripts)]
     (player/run (-> files flatten vec)))))

(defn-spec player-test ::vt/any
  ([body ::vt/any] (player-test :run-artest/default-body default-scripts-path (baby.pat.vt/qid) body))
  ([script-id ::vt/kw-or-str body ::vt/any] (player-test :run-artest/default-body default-scripts-path script-id body))
  ([prefix ::vt/str script-id ::vt/kw-or-str body ::vt/any]
   (player-test :run-artest/default-body prefix script-id body))
  ([test-body-id ::vt/qkw prefix ::vt/str script-id ::vt/kw-or-str body ::vt/any]
   (let [test-body (TPL/render {:tpl/id test-body-id
                                :body body})
         path (get-script-file-path prefix script-id)
         _ (fs/create-dirs (fs/parent path))]
     (spit path test-body))))

(defn-spec blank-script-base ::vt/map []
  {:script/id (baby.pat.vt/qid)
   :script/test-template :run-artest/default-body
   :script/created-at (tick.core/now)
   :script/path-prefix default-scripts-path
   :script/traits #{:script/all}})

(defn-spec as-script ::vt/map
  ([m ::vt/any]
   (if-not (map? m)
     (as-script {:body m})
     (let [{:script/keys [id path-prefix test-template body] :as script-description} (merge (blank-script-base) (baby.pat.vt/add-kw-ns :script m))
           script-path  (get-script-file-path path-prefix id)
           test-body (TPL/render {:tpl/id test-template
                              :body body})]
       (assoc script-description :script/path script-path :script/test-body test-body))))
  ([script-id ::vt/kw-or-str body ::vt/any] (as-script {:script/id script-id :script/body body}))
  ([test-template-id ::vt/kw script-id ::vt/kw-or-str body ::vt/any]
   (as-script {:script/id script-id :script/test-template test-template-id :script/body body})))

(def run-artest-functions {:testing/quick-test QT
                           :testing/player-test player-test
                           :testing/as-script as-script})
