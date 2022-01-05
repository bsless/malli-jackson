(ns io.github.bsless.malli-jackson.util)

(defn index-by
  [f coll]
  (reduce (fn [m x] (assoc m (f x) x)) {} coll))

(defn queue
  [& xs]
  (into (clojure.lang.PersistentQueue/EMPTY) xs))

(defn bfs
  "visit :: (elem queue visited accum) -> [queue accum]"
  [c visit init]
  (loop [q (queue c)
         visited #{}
         accum init]
    (if (zero? (count q))
      accum
      (let [elem (peek q)]
        (if (visited elem)
          (recur (pop q) visited accum)
          (let [q (pop q)
                [q accum] (visit elem q visited accum)]
            (recur q (conj visited elem) accum)))))))

(defn maybe-update
  [m k f]
  (let [v (m k)]
    (if (nil? v)
      m
      (assoc m k (f v)))))
