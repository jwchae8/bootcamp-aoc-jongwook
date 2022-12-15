(ns aoc2020_1)

(def input-lines (->>
                   "resources/aoc2020/day1"
                   slurp
                   clojure.string/split-lines
                   (map parse-long)))
(def indexed-input-lines (map-indexed vector input-lines))

(defn product-of-two-entries-sum-n-using-for
  [indexed-nums n]
  (-> (for [[idx1 num1] indexed-nums
            [idx2 num2] indexed-nums
            :while (> idx1 idx2)
            :when (= (+ num1 num2)
                     n)]
       (* num1 num2))
      first))

(product-of-two-entries-sum-n-using-for indexed-input-lines 2020)

(defn product-of-three-entries-sum-n-using-for
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

(product-of-three-entries-sum-n-using-for indexed-input-lines 2020)


(defn product-of-two-entries-sum-n
  [nums n]
  (reduce (fn [x y] (if (x (- n y))
                      (reduced (* y (- n y)))
                      (conj x y))) #{} nums))

(product-of-two-entries-sum-n input-lines 2020)

;(defn product-of-three-entries-sum-n
;  [nums n]
;  (reduce (fn [x y] (if (product-of-two-entries-sum-n (rest x) (- n (first x)))
;                      (reduced (* y (- 2020 y)))
;                      (rest y))) nums nums))