(ns aoc2018_7)

(def input-lines (->
                   "resources/day7"
                   slurp
                   clojure.string/split-lines))

(def input-pattern #"([A-Z]) must be finished before step ([A-Z])")
(map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (repeat #{}))
(map (partial re-find input-pattern) input-lines)
(->> input-lines
     (map (partial re-find input-pattern))
     (map rest)
     ((fn [y] (reduce (fn [x [s t]] (merge-with into x {t #{s}})) (zipmap (map first y) (repeat #{})) y))))
     ;((fn [y] (reduce (fn [x [s t]] (merge-with into x {s #{t}})) (zipmap (map second y) (repeat #{}))))))
     ;(reduce (fn [x y] (merge-with into x y)) (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (repeat #{}))))

(first '("O" "N"))
(second '("O" "N"))
(merge-with into {"O" #{"N"}} {(first '("K" "Y"))
                               #{(second '("K" "Y"))}})

(def graph {"T" #{"S" "R" "U"},
            "K" #{"B"},
            "Q" #{"H" "W"},
            "L" #{"Q" "J" "Z" "E" "F" "P" "U" "O" "N" "A" "W" "D"},
            "G" #{"R" "A" "I"},
            "J" #{},
            "M" #{"T" "Q" "G" "Y" "H" "U" "X" "A" "I"},
            "S" #{},
            "Y" #{"C" "B"},
            "Z" #{"Q" "M" "H" "R" "C" "F" "P" "V" "O" "X" "N" "A"},
            "H" #{"R"},
            "E" #{"G" "M" "Z" "R" "C" "F" "P" "X" "N" "A" "I"},
            "R" #{},
            "C" #{"S"},
            "F" #{"T" "M" "C" "U" "O" "A" "D"},
            "B" #{"S"},
            "P" #{"T" "Y" "O" "A" "D"},
            "V" #{"B"},
            "U" #{},
            "O" #{"S" "Y" "H" "D"},
            "X" #{"Q" "S" "C" "P" "O" "I" "W"},
            "N" #{"G" "M" "H" "C" "P" "U" "O" "I"},
            "A" #{"K" "Q" "D"},
            "I" #{"Q" "J" "S"},
            "W" #{"S" "V"},
            "D" #{"J" "U" "W"}})
(first (sort (map key (filter #(= (count (val %)) 0) graph))))
(map key (filter #(= (count (val %)) 0) graph))
(first (sort '("L" "M" "A")))


(defn topological-sort [graph] (let [no-dependency (first (sort (map key (filter #(= (count (val %)) 0) graph))))]
                                 (->> (dissoc graph no-dependency)
                                      (reduce (fn [x [a b]] (merge x {a (disj b no-dependency)}) ) {}))))
(take 26 (iterate topological-sort graph))
(apply str (reduce (fn [x y] (concat x (first (sort (map key (filter #(= (count (val %)) 0) y)))))) "" (take 26 (iterate topological-sort graph))))

