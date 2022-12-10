(ns cloud-descriptor.utils)

(defn find-direct-child
  [tree keyword]
  (reduce (fn [acc item]
            (if (= (first item) keyword)
              item
              acc))
          nil
          (rest tree)))

(defn find-first
  [pred coll]
  (when-let [s (seq coll)]
    (if (pred (first s))
      (first s)
      (recur pred (next s)))))

(defn replace-nth
  [idx item lst]
  (when (seq lst)
    (if (<= idx 0)
      (cons item (rest lst))
      (cons (first lst)
            (replace-nth (dec idx) item (rest lst))))))

(defn maybe-assoc
  [map key val]
  (if (contains? map key)
    (assoc map key val)
    map))

(defn maybe-assoc-in
  [m [k & ks] v]
  (if ks
    (maybe-assoc m k (maybe-assoc-in (get m k) ks v))
    (maybe-assoc m k v)))

(defn split-by
  [pred coll]
  [(filter pred coll)
   (filter (complement pred) coll)])

