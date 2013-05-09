(ns databinder.template
  (:require
   [clojure.java.io :as io]
   [hiccup.core :as hic])
  (:import
   [org.mozilla.javascript Context ScriptableObject Function NativeArray]))

(def js-env (atom nil))

(defn template-env []
  (let [cx (Context/enter)
        scope (.initStandardObjects cx)]
    (.evaluateReader cx scope (io/reader (io/resource "mustache.js")) "mustache.js" 1 nil)
    (.evaluateReader cx scope (io/reader (io/resource "underscore.js")) "mustache.js" 1 nil)
    (.evaluateReader cx scope (io/reader (io/resource "templates.js")) "templates.js" 1 nil)
    (swap! js-env (fn [old] {:context cx :scope scope}))))

(defn mapper [obj vals]
  (doseq [key (keys vals)]
    (ScriptableObject/putProperty obj key (get vals key))))

(defn template [js]
  (fn [resource bindings url params vals]
    (let [cx (Context/enter)
          shared-scope (:scope @js-env)
          scope (.newObject cx shared-scope)
          a (.setPrototype scope shared-scope)
          b (.setParentScope scope nil)]

      (mapper scope params)

      (ScriptableObject/putProperty scope "uri" resource)
      (ScriptableObject/putProperty scope "binding" bindings)
      (ScriptableObject/putProperty scope "url" url)
      (ScriptableObject/putProperty scope "children"
                                    (new NativeArray
                                         (to-array (map (fn [val] (hic/html val)) vals))))

      (.evaluateString cx scope "var ret=\"\"; function emit(val){ret=val;};" "<cmd>" 1 nil)
      (.evaluateString cx scope js "<cmd>" 1 nil) ;;TODO sort out line numbers
      ;;(.evaluateString cx scope (str "var ret = stuff.resource") "<cmd>" 1 nil)
      (Context/exit)
      (.get scope "ret" scope))))


(template-env)
