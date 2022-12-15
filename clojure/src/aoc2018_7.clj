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

(defn no-dependency?
  [job-to-dependencies]
  (-> job-to-dependencies
      val
      count
      zero?))

(defn remove-jobs-from-graph
  [graph jobs]
  (->> (apply dissoc graph jobs)
       (reduce (fn [graph-after-removal [job dependency]]
                 (merge graph-after-removal {job (apply disj dependency jobs)})) {})))
(defn available-worker-count
  [{:keys [worker-count
           scheduled-jobs]}]
  (- worker-count (count scheduled-jobs)))

(defn job-to-finished-time
  [])
(defn finish-jobs
  [{:keys [scheduled-jobs]
    :as state}]
  (let [finished-jobs (->> scheduled-jobs
                           (apply min-key val))]
    (assoc state :scheduled-jobs (apply dissoc scheduled-jobs (map key finished-jobs))
                 :current-timestamp (-> finish-jobs
                                        first
                                        val))))

(defn assign-jobs
  [{:keys [dependency-graph
           current-timestamp
           scheduled-jobs
           job-elapsed-time]
    :as state}]
  (let [no-dependency-jobs (->> dependency-graph
                                (filter no-dependency?)
                                (map key)
                                sort
                                (take (available-worker-count state)))]
    (assoc state :scheduled-jobs (merge scheduled-jobs (update-vals
                                                         (select-keys job-elapsed-time no-dependency-jobs)
                                                         #(+ % current-timestamp)))
                 :dependency-graph (remove-jobs-from-graph dependency-graph
                                                           no-dependency-jobs))))


(defn update-state
  [{:keys [scheduled-jobs]
    :as state}]
  (-> state
      assign-jobs
      finish-jobs))

(min-key :a '({:a 1 :b 2} {:a 0 :b 1}))
(apply min-key key {1 2 2 3 3 4})

(apply dissoc {1 2 2 3 3 4} '(1 2))
(apply disj #{1 2 3 4} '(1 2))