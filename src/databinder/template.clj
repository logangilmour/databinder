(ns databinder.template
  (:require
   [clojure.java.io :as io]
   [hiccup.core :as hic])
  (:import
   [org.mozilla.javascript Context ScriptableObject Function NativeArray NativeJSON]))

(def js-env (atom nil))

(defn template-env []
  (let [cx (Context/enter)
        scope (.initStandardObjects cx)]
    (.evaluateReader cx scope (io/reader (io/resource "mustache.js")) "mustache.js" 1 nil)
    (.evaluateReader cx scope (io/reader (io/resource "underscore.js")) "mustache.js" 1 nil)
    (.evaluateReader cx scope (io/reader "templates.js") "templates.js" 1 nil)
    (swap! js-env (fn [old] {:context cx :scope scope}))))

(defn js-array [coll]
  (new NativeArray (to-array coll)))

(defn mapper [obj vals]
  (doseq [[key val] vals]
    (if (seq? val)
      (ScriptableObject/putProperty obj key (js-array val))
      (ScriptableObject/putProperty obj key val))))

(defn template [js resource bindings url params]
  (let [cx (Context/enter)
        shared-scope (:scope @js-env)
        scope (.newObject cx shared-scope)
        a (.setPrototype scope shared-scope)
        b (.setParentScope scope nil)]

    (mapper scope params)

    (ScriptableObject/putProperty scope "uri" resource)
    (ScriptableObject/putProperty scope "binding" bindings)
    (ScriptableObject/putProperty scope "url" url)

    (.evaluateString cx scope "var ret=\"\"; function emit(val){ret=val;};" "<cmd>" 1 nil)
    (.evaluateString cx scope js "<cmd>" 1 nil) ;;TODO sort out line numbers
    ;;(.evaluateString cx scope (str "var ret = stuff.resource") "<cmd>" 1 nil)
    (Context/exit)
    (.get scope "ret" scope)))

(defn stringify [val]
  (let [cx (Context/enter)
        shared-scope (:scope @js-env)
        scope (.newObject cx shared-scope)
        ret (NativeJSON/stringify cx scope val nil nil)]
    (Context/exit)
    ret))
