(ns cloud-descriptor.utils)

(defn find-direct-child
  [tree keyword]
  (reduce (fn [acc item]
            (if (= (first item) keyword)
              item
              acc))
          nil
          (rest tree)))

