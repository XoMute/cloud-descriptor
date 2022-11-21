(ns cloud-descriptor.utils)

(defn find-direct-child
  [tree keyword]
  (reduce (fn [acc item]
            (if (= (first item) keyword)
              item
              acc))
          nil
          (rest tree)))

(defn find-attribute ;; TODO: write in disser that in case of duplicate attributes no error will be thrown
  [attributes name]
  (->> attributes
       (filter #(= (:name %) name))
       last))

