(ns io.github.bsless.malli-jackson
  (:require
   [io.github.bsless.malli-jackson.util :as u]
   [io.github.bsless.malli-jackson.parser :as parser]
   [io.github.bsless.malli-jackson.emitter :as emitter]))

(defn parse
  [c]
  (u/bfs c parser/visit-class []))

(comment
  (def parsed (parse org.apache.druid.query.Query)))

(defn emit
  [parsed]
  (reduce emitter/emit {} parsed))

(comment
  (def registry (emit parsed)))

(defn parse+emit
  [s]
  (-> s Class/forName parse emit))
