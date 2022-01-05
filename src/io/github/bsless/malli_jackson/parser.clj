(ns io.github.bsless.malli-jackson.parser
  (:require
   [camel-snake-kebab.core :as csk]
   [io.github.bsless.malli-jackson.util :as u]
   [clojure.string :as str])
  (:import
   (com.fasterxml.jackson.annotation
    JsonTypeInfo$As
    JsonTypeInfo$Id
    JsonProperty$Access
    OptBoolean
    JsonTypeInfo
    JsonTypeName
    JacksonInject
    JsonSubTypes
    JsonProperty)
   (javax.annotation Nullable)
   (java.lang.annotation Annotation)
   (java.lang Deprecated)
   (java.util Map List Set)
   (java.lang.reflect Type TypeVariable ParameterizedType Constructor Method Modifier)))

;;; Predicates

(defn abstract? [^Class c] (Modifier/isAbstract (.getModifiers c)))
(defn interface? [^Class c] (Modifier/isInterface (.getModifiers c)))
(defn type? [o] (instance? Type o))
(defn type-variable? [o] (instance? TypeVariable o))
(defn colletcion? [o] (instance? java.util.Collection o))
(defn parametrized-type? [o] (instance? ParameterizedType o))

(comment
  (abstract? org.apache.druid.query.Query)
  (abstract? org.apache.druid.query.timeseries.TimeseriesQuery))

(defn- class-annotations [^Class clazz] (.getAnnotations clazz))

(defn- annotation-type ^Class [^Annotation ann] (.annotationType ann))

(defmulti -parse-annotation (fn [^Annotation a] (annotation-type a)))

(def base-classes
  #{Map List String Long Integer Set})

(defn- parse-annotations
  [^Class c]
  (when-let [anns (seq (map -parse-annotation (class-annotations c)))]
    (reduce conj anns)))

(comment
  (parse-annotations org.apache.druid.query.Query))

(defn- annotation->map
  ([a] (annotation->map a (csk/->kebab-case (.getSimpleName (annotation-type a)))))
  ([a prefix]
   (let [methods (.getDeclaredMethods (annotation-type a))]
     (into {} (map (fn [^Method m] [(keyword prefix (.getName m)) (.invoke m a nil)])) methods))))

(defprotocol IEnumParser
  "Parse annotations defined as enums."
  (-parse-enum [e]))

(extend-protocol IEnumParser
  JsonTypeInfo$As
  (-parse-enum [e]
    (keyword "json-type-info.as" (str/lower-case (str e))))

  JsonTypeInfo$Id
  (-parse-enum [e]
    (keyword "json-type-info.as" (str/lower-case (str e))))

  JsonProperty$Access
  (-parse-enum [e]
    (keyword "json-property.access" (str/lower-case (str e))))

  OptBoolean
  (-parse-enum [e]
    (keyword "opt-boolean" (str/lower-case (str e)))))

(defmethod -parse-annotation JsonTypeInfo
  [a]
  (-> a
      (annotation->map "json-type-info")
      (update :json-type-info/include -parse-enum)
      (update :json-type-info/use -parse-enum)))

(defmethod -parse-annotation JsonTypeName [a]
  (-> a
      (annotation->map "json-type-name")))

(defmethod -parse-annotation JacksonInject [a]
  (-> a
      annotation->map
      (u/maybe-update :useInput -parse-enum)))

(defmethod -parse-annotation JsonSubTypes [a]
  (-> a
      (annotation->map "json-sub-types")
      (update :json-sub-types/value (partial mapv annotation->map))))

(defmethod -parse-annotation JsonProperty
  [a]
  (-> a
      (annotation->map "json-property")
      (u/maybe-update :access -parse-enum)))

(defmethod -parse-annotation Nullable [_]
  {:nullable? true
   :type :nullable})

(defmethod -parse-annotation Deprecated [_]
  {:deprecated? true
   :type :deprecated})

(defn- parse-type
  [^Type t]
  (cond
    (class? t) {:rawType t}
    (parametrized-type? t) (update (bean t) :actualTypeArguments (partial mapv parse-type))))

(defprotocol IAnnotationsParse
  (-parse-annotations [this] "Parse the annotations on a method, constructor, etc."))

(extend-protocol IAnnotationsParse
  Constructor
  (-parse-annotations [constructor]
    {:declared-annotations (vec (.getDeclaredAnnotations constructor))
     :parameter-annotations (vec (.getParameterAnnotations constructor))
     :generic-parameter-types (vec (.getGenericParameterTypes constructor))})
  Method
  (-parse-annotations [method]
    {:declared-annotations (vec (.getDeclaredAnnotations method))
     :parameter-annotations (vec (.getParameterAnnotations method))
     :generic-parameter-types (vec (.getGenericParameterTypes method))}))

(defn- -json-creator?
  "True if method has JSON creator annotation"
  [m]
  (some #(= com.fasterxml.jackson.annotation.JsonCreator (annotation-type %))
        (seq (.getDeclaredAnnotations m))))

(defn- find-json-creators
  [^Class c]
  (filter -json-creator? (concat (.getDeclaredMethods c) (.getDeclaredConstructors c))))

(comment
  (find-json-creators org.apache.druid.query.Query)
  (find-json-creators org.apache.druid.query.timeseries.TimeseriesQuery))

(defn- -parse-json-creator
  [constructor]
  (let [{:keys [parameter-annotations generic-parameter-types]}
        (-parse-annotations constructor)
        params-annotations (map (fn [arr] (reduce conj {} (map -parse-annotation arr))) parameter-annotations)
        generic-type-params (mapv parse-type generic-parameter-types)
        args (mapv (fn [ann t] (merge ann t)) params-annotations generic-type-params)]
    {:constructor constructor
     :args args}))

(comment
  (map -parse-json-creator (find-json-creators org.apache.druid.query.timeseries.TimeseriesQuery)))

(def xf (comp
         (filter map?)
         (map :rawType)
         (remove nil?)))

(defn visit-class
  [^Class c q visited accum]
  (let [res
        (merge
         (parse-annotations c)
         (some-> c find-json-creators first -parse-json-creator))
        cname (.getName c)
        json-sub-types (:json-sub-types/value res)
        constructor (:constructor res)
        cargs (:args res)
        type-name (:json-type-name/value res)]
    (cond
      (empty? res) [q accum]
      json-sub-types [(into q (map :type/value) json-sub-types)
                      (conj accum (assoc res :class cname))]
      constructor
      (let [types (tree-seq coll? (fn [o] (cond (map? o) (:actualTypeArguments o) (vector? o) o)) cargs)]
        [(into q (comp xf (remove visited)) types)
         (conj accum (assoc res :class cname))])

      type-name ;; type-alias
      [q (conj accum (assoc res :class cname))]
      :else
      (do
        (println "ERROR:" c)
        [q accum]))))

(comment
  (visit-class org.apache.druid.query.ordering.StringComparator (u/queue) #{} [])
  (visit-class org.apache.druid.query.Query (u/queue) #{} [])
  (visit-class org.apache.druid.query.timeseries.TimeseriesQuery (u/queue) #{} [])
  (visit-class org.apache.druid.query.extraction.UpperExtractionFn (u/queue) #{} []))
