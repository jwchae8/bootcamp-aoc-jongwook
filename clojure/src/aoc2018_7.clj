(ns aoc2018_7)

(def input-lines
  "
  example
  Step S must be finished before step B can begin.
  "
  (->
    "resources/day7"
    slurp
    clojure.string/split-lines))

(def input-pattern #"([A-Z]) must be finished before step ([A-Z])")
(def alphabet-set '(:A :B :C :D :E :F :G :H :I :J :K :L :M :N :O :P :Q :R :S :T :U :V :W :X :Y :Z))
(defn parse
  "
  각 라인을 파싱하는 함수
  input: 파일 입출력 상 한 줄
  output: dependency 관계의 두 job id
  example
  Step S must be finished before step B can begin. -> (:S :B)
  "
  [line]
  (->> line
       (re-find input-pattern)
       rest
       (map keyword)))

(comment
  (parse "Step S must be finished before step B can begin."))

(defn graph
  "
  파싱된 dependency 관계를 이용해 adjacency list로 표현되는 그래프 구성
  input: dependency 관계의 두 job을 의미하는 두 keyword가 들어있는 리스트의 리스트
         (prerequisite job)의 순서
  output: key는 job이고 value는 그 job이 실행되기 위한 prerequisite들의 set인 hashmap
  example
  ((:A :B)) -> {:A #{} :B #{A}}
  "
  [dependencies]
  (reduce (fn [graph [prerequisite job]]
            (merge-with into graph {job #{prerequisite}}))
          (zipmap (map first dependencies) (repeat #{}))
          dependencies))

(comment
  (graph '((:A :B))))

(defn no-dependency?
  "
  해당 job보다 먼저 수행돼야 하는 job이 더 이상 없는지 체크하는 함수
  input: key는 job이고 value는 그 job보다 먼저 실행되야 하는 job들의 set인 hashmap
  output: 남은 job이 없으면 true 있으면 false
  example
  {:A #{}} -> true       {:B #{A}} -> false
  "
  [job-to-dependencies]
  (-> job-to-dependencies
      val
      empty?))
(comment
  (no-dependency? (first {:A #{}})))
(comment
  (no-dependency? (first {:B #{:A}})))

(defn remove-jobs-from-graph
  "
  그래프로부터 job을 제거하는 함수(key중에 job을 제거)
  input: graph를 의미하는 hashmap과 job id의 list
  output: graph로부터 job id의 list에 해당하는 key가 제거된 hashmap
  example
  {:A #{:B :C} :B #{:C} :C #{} :D #{}} (:C) -> {:A #{:B :C} :B #{:C}}
  "
  [graph jobs]
  (apply dissoc graph jobs))

(comment
  (remove-jobs-from-graph {:A #{:B :C} :B #{:C} :C #{} :D #{}} '(:C :D)))

(defn remove-dependencies-from-graph
  "
  그래프로부터 dependency를 제거하는 함수(val에서 job을 제거)
  input: graph를 의미하는 hashmap과 job id의 list
  output: graph로부터 각 value에 해당하는 set에서 job id들이 제거되고 남은 hashmap
  example
  {:A #{:B :C} :B #{:C}} (:C) -> {:A #{:B} :B #{}}
  "
  [graph removed-dependencies]
  (update-vals graph (fn [dependencies]
                       (apply disj dependencies removed-dependencies))))

(comment
  (remove-dependencies-from-graph {:A #{:B :C} :B #{:C}} '(:C)))

(defn available-worker-count
  "
  현재 상태를 받아서 작업 가능한 인원 수를 구하는 함수
  "
  [{:keys [worker-count
           scheduled-jobs]}]
  (- worker-count (count scheduled-jobs)))

(defn job-to-required-time
  "
  작업시간을 각 job id에 매핑해주는 함수
  input: 작업 시간을 나타내는 lazy sequence
  output: key가 job id고 작업 시간이 value인 hashmap
  (iterate inc 1) -> {:A 1 :B 2 :C 3 ... :Z 26}
  "
  [required-time]
  (zipmap alphabet-set required-time))

(comment
  (job-to-required-time (repeat 1)))
(comment
  (job-to-required-time (iterate inc 61)))

(defn finish-jobs
  "
  가장 일찍 종료되는 Job들에 대한 종료처리를 하는 함수
  input: 현재 state
  output: job들을 종료시킨 후의 state
  "
  [{:keys [scheduled-jobs
           job-history
           dependency-graph]
    :as state}]
  (let [[finished-time finished-jobs] (->> scheduled-jobs
                                           (group-by val)
                                           (apply min-key key))
        finished-job-ids (->> finished-jobs
                              (map first)
                              sort)]
    (assoc state :scheduled-jobs (apply dissoc scheduled-jobs finished-job-ids)
                 :current-timestamp finished-time
                 :job-history (apply conj job-history finished-job-ids)
                 :dependency-graph (remove-dependencies-from-graph dependency-graph finished-job-ids))))

(defn assign-jobs
  "
  수행 가능한 job들을 시작하는 함수
  input: 현재 state
  output: job들을 시작시킨 후의 state
  "
  [{:keys [dependency-graph
           current-timestamp
           scheduled-jobs
           job-to-required-time]
    :as state}]
  (let [no-dependency-jobs (->> dependency-graph
                                (filter no-dependency?)
                                (map key)
                                sort
                                (take (available-worker-count state)))
        new-scheduled-jobs (-> job-to-required-time
                               (select-keys no-dependency-jobs)
                               (update-vals #(+ % current-timestamp)))]
    (assoc state :scheduled-jobs (merge scheduled-jobs new-scheduled-jobs)
                 :dependency-graph (remove-jobs-from-graph dependency-graph
                                                           no-dependency-jobs))))

(defn jobs-remaining?
  "
  남은 job이 있는지 확인하는 함수
  input: state
  output: 주어진 state에 남은 job 혹은 아직 진행 중인 job이 있으면 true, 아니면 false
  "
  [{:keys [dependency-graph
           scheduled-jobs]}]
  (or (seq dependency-graph) (seq scheduled-jobs)))

(defn init-state
  "
  초기 state를 만들어주는 함수
  input: 최대 작업자 수, 각 job당 작업 시간, dependency 목록
  output: {:dependency-graph dependency 목록을 기반으로 만든 adjacency list
           :current-timestamp 현재 상태가 나타내고 있는 시간
           :scheduled-jobs 진행 중인 job id를 key로 하고 종료시간을 value로 하는 hashmap
           :job-to-required-time job별 걸리는 시간
           :worker-count 최대 작업자 수
           :job-history job 처리 이력}
  "
  [worker-count required-time dependencies]
  {:dependency-graph (graph dependencies)
   :current-timestamp 0
   :scheduled-jobs {}
   :job-to-required-time (job-to-required-time required-time)
   :worker-count worker-count
   :job-history []})

(defn update-state
  "
  작업 수행의 메인 루프
  "
  [state]
  (-> state
      assign-jobs
      finish-jobs))

(defn state-after-all-jobs-done
  [state]
  (first (drop-while jobs-remaining? state)))

;; part1
(defn single-worker-immediately-finished-job
  [input-lines]
  (->> input-lines
       (map parse)
       (init-state 1 (repeat 1))
       (iterate update-state)
       state-after-all-jobs-done
       :job-history
       (map name)
       (apply str)))
(comment
  (single-worker-immediately-finished-job input-lines))

;; part2
(defn multi-worker-time-taking-job
  [input-lines]
  (->> input-lines
       (map parse)
       (init-state 5 (iterate inc 61))
       (iterate update-state)
       state-after-all-jobs-done
       :current-timestamp))

(comment
  (multi-worker-time-taking-job input-lines))
