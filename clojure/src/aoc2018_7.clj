(ns aoc2018_7)

(def input-lines (->
                   "resources/day7"
                   slurp
                   clojure.string/split-lines))

(def input-pattern #"([A-Z]) must be finished before step ([A-Z])")
(def alphabet-set (map keyword (clojure.string/split "ABCDEFGHIJKLMNOPQRSTUVWXYZ" #"")))
(defn parse
  [line]
  (->> line
       (re-find input-pattern)
       rest
       (map keyword)))
(defn graph
  [dependencies]
  (reduce (fn [graph [prerequisite job]]
            (merge-with into graph {job #{prerequisite}}))
          (zipmap (map first dependencies) (repeat #{}))
          dependencies))
(defn no-dependency?
  [job-to-dependencies]
  (-> job-to-dependencies
      val
      count
      zero?))

(defn remove-jobs-from-graph
  [graph jobs]
  (apply dissoc graph jobs))
       ;(reduce (fn [graph-after-removal
       ;             [job dependency]]
       ;          (merge graph-after-removal {job
       ;                                      (apply disj dependency removed-jobs)})) {})))

(defn remove-dependencies-from-graph
  [graph dependencies]
  (reduce (fn [graph-after-removal
               [job dependency]]
            (merge graph-after-removal {job
                                        (apply disj dependency dependencies)})) {} graph))

(defn available-worker-count
  [{:keys [worker-count
           scheduled-jobs]}]
  (- worker-count (count scheduled-jobs)))

(defn job-to-required-time
  [required-time]
  (zipmap alphabet-set required-time))

(defn finish-jobs
  [{:keys [scheduled-jobs
           job-history
           dependency-graph]
    :as state}]
  (let [finished-jobs (->> scheduled-jobs
                           (group-by val)
                           (apply min-key key))]
    (assoc state :scheduled-jobs (->> finished-jobs
                                      val
                                      (map first)
                                      (apply dissoc scheduled-jobs))
                 :current-timestamp (key finished-jobs)
                 :job-history (apply conj job-history (sort (map first (val finished-jobs))))
                 :dependency-graph (remove-dependencies-from-graph dependency-graph (map first (val finished-jobs))))))

(defn assign-jobs
  [{:keys [dependency-graph
           current-timestamp
           scheduled-jobs
           job-to-required-time]
    :as state}]
  (let [no-dependency-jobs (->> dependency-graph
                                (filter no-dependency?)
                                (map key)
                                sort
                                (take (available-worker-count state)))]
    (assoc state :scheduled-jobs (merge scheduled-jobs (update-vals
                                                         (select-keys job-to-required-time no-dependency-jobs)
                                                         #(+ % current-timestamp)))
                 :dependency-graph (remove-jobs-from-graph dependency-graph
                                                           no-dependency-jobs))))

(defn init-state
  [worker-count required-time dependencies]
  {:dependency-graph (graph dependencies)
   :current-timestamp 0
   :scheduled-jobs {}
   :job-to-required-time (job-to-required-time required-time)
   :worker-count worker-count
   :job-history []})

(defn update-state
  [state]
  (-> state
      assign-jobs
      finish-jobs))

(defn single-worker-immediate-finished-job
  [input-lines]
  (->> input-lines
       (map parse)
       (init-state 1 (repeat 1))
       (iterate update-state)
       (drop-while #(seq (:dependency-graph %)))
       first
       :job-history
       (map name)
       (apply str)))
(comment
  (single-worker-immediate-finished-job input-lines))
(graph (map parse input-lines))
(defn multi-worker-time-taking-job
  [input-lines]
  (->> input-lines
       (map parse)
       (init-state 5 (iterate inc 61))
       (iterate update-state)
       (drop-while #(or (seq (:dependency-graph %)) (seq (:scheduled-jobs %))))
       first
       :current-timestamp))

(job-to-required-time (iterate inc 61))
(comment
  (multi-worker-time-taking-job input-lines))
