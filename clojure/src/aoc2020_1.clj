(ns aoc2020_1)

(def input-lines (->>
                   "resources/aoc2020/day1"
                   slurp
                   clojure.string/split-lines
                   (map parse-long)))
(def indexed-input-lines (map-indexed vector input-lines))

(defn product-of-two-entries-sum-n
  [indexed-nums n]
  (-> (for [[idx1 num1] indexed-nums
            [idx2 num2] indexed-nums
            :while (> idx1 idx2)
            :when (= (+ num1 num2)
                     n)]
       (* num1 num2))
      first))

(comment
  (product-of-two-entries-sum-n indexed-input-lines 2020))

(defn product-of-three-entries-sum-n
  [indexed-nums n]
  (-> (for [[idx1 num1] indexed-nums
            [idx2 num2] indexed-nums
            [idx3 num3] indexed-nums
            :while (and (> idx1 idx2)
                        (> idx2 idx3))
            :when (= (+ num1 num2 num3)
                     n)]
        (* num1 num2 num3))
      first))

(comment
  (product-of-three-entries-sum-n indexed-input-lines 2020))


(defn product-of-two-entries-sum-n-using-set
  [nums n]
  (reduce (fn [x y] (if (x (- n y))
                      (reduced (* y (- n y)))
                      (conj x y))) #{} nums))
(comment
  (product-of-two-entries-sum-n-using-set input-lines 2020))

(defn product-of-three-entries-sum-n-using-set
  [nums n]
  (reduce (fn
            [x y]
            (let [two-entries (product-of-two-entries-sum-n-using-set (rest x) (- n y))]
              (if (integer? two-entries) (reduced (* y two-entries)) (rest x)))) nums nums))

(comment
  (product-of-three-entries-sum-n-using-set input-lines 2020))
