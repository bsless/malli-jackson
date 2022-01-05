(ns io.github.bsless.malli-jackson.emitter
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.string :as str]))

(def builtins
  {java.lang.String :string
   java.lang.Boolean :boolean
   java.lang.Integer :integer
   java.lang.Long :integer
   java.lang.Object :any
   "boolean" :boolean
   "float" :double
   "int" :integer
   "long" :integer
   "java.lang.Boolean" :boolean
   "java.lang.Integer" :integer
   "java.lang.Object" :any
   "java.lang.String" :string
   })

(def collections
  {java.util.List :sequential
   java.util.Map :map-of})

(defn prename
  [s]
  (let [s (if (string? s) s (.getName ^Class s))]
    (if-let [k (get builtins s)]
      k
      (let [v (str/split s #"\.")
            name (peek v)
            package (str/join "." (pop v))]
        (keyword package (csk/->kebab-case name))))))

(defn emit-type
  [{:keys [rawType actualTypeArguments] :as m}]
  (cond

    (contains? collections rawType)
    (into [(get collections rawType)]
          (map emit-type)
          actualTypeArguments)

    (contains? builtins rawType) (get builtins rawType)

    (.isArray ^Class rawType) [:sequential (emit-type {:rawType (.getComponentType ^Class rawType)})]

    :else (if rawType (prename rawType) (println m)))
  )

(defn emit
  [reg
   {:keys [constructor
           args
           class]
    :as m}]
  (let [argc (count args)
        one-arg? (= 1 argc)
        arg (when one-arg? (first args))
        type-name (:json-type-name/value m)
        sub-types (:json-sub-types/value m)]
    (conj
     reg
     (cond

       (and constructor type-name one-arg?)
       [(prename class)
        [:map
         [(csk/->kebab-case-keyword type-name) (emit-type arg)]]]

       (and constructor one-arg? (not (:json-property/value arg)))
       [(prename class) (emit-type arg)]

       constructor
       [(prename class)
        (into [:map]
              (map (fn [{:json-property/keys [value] :as m}]
                     [(when value (csk/->kebab-case-keyword value)) (emit-type m)]))
              args)]

       (:json-type-info/use m)
       (let [{:json-type-info/keys [property #_#_#_include use visible]} m]
         [(prename class)
          (into
           [:multi {:dispatch (keyword property)}]

           (map (fn [{:type/keys [name value]}] [name (prename value)]))
           sub-types)])

       type-name
       [type-name (prename class)]

       :else (throw (ex-info "No matching case found" m))))))
