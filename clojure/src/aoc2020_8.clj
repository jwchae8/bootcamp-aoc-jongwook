(ns aoc2020_8)


(def input-lines
  "
  example
  nop +355
  acc +46
  jmp +42
  "
  (->
    "resources/aoc2020/day8"
    slurp
    clojure.string/split-lines))

(defn parse
  "
  input: 입력의 각 라인
  output: 라인 내 내용에 맞는 {:operation 명령 :argument 명령값} 형태의 hashmap
  example
  nop +355 -> {:operation \"nop\" :argument 355}
  acc +46 -> {:operation \"acc\" :argument 46}
  jmp +42 -> {:operation \"jmp\" :argument 42}
  "
  [line]
  (-> line
    (clojure.string/split #" ")
    ((fn [[op arg]] {:operation (keyword op)
                     :argument (parse-long arg)}))))
(comment
  (parse "nop +355")
  (parse "acc +46")
  (parse "jmp +42"))

(defn init-program
  [operations]
  "
  input: 프로그램 내 명령들 {:operation 명령 :argument 명령값}의 벡터
  output: 상태값을 나타내는 hashmap
          {:operations input의 명령 벡터
           :accumulator 0(현재 누산기에 쌓인 값)
           :program-counter 0(현재 실행 중인 명령줄)
           :execution-history #{}(여태까지 실행한 명령들)}
  "
  {:operations operations
   :accumulator 0
   :program-counter 0
   :execution-history #{}})

(defn execute-jmp
  "
  input: 현재 상태값을 나타내는 hashmap
  output: jmp 명령을 수행하고 난 이후 상태의 hashmap
  "
  [{:keys [operations
           execution-history
           program-counter]
    :as state}]
  (let [current-operation (nth operations program-counter)
        next-program-counter (:argument current-operation)]
    (assoc state :program-counter (+ program-counter next-program-counter)
                 :execution-history (conj execution-history program-counter))))

;; defmulti - defmethod로 해보기
;; execute-**들끼리 공통분모를 한 함수로 묶고 state 업데이트하는 부분만 갈라보기
(defn execute-acc
  "
  input: 현재 상태값을 나타내는 hashmap
  output: acc 명령을 수행하고 난 이후 상태의 hashmap
  "
  [{:keys [operations
           execution-history
           program-counter
           accumulator]
    :as state}]
  (let [current-operation (nth operations program-counter)
        addition (:argument current-operation)]
    (assoc state :program-counter (inc program-counter)
                 :accumulator (+ accumulator addition)
                 :execution-history (conj execution-history program-counter))))

(defn execute-nop
  "
  input: 현재 상태값을 나타내는 hashmap
  output: nop 명령을 수행하고 난 이후 상태의 hashmap
  "
  [{:keys [execution-history
           program-counter]
    :as state}]
  (assoc state :program-counter (inc program-counter)
               :execution-history (conj execution-history program-counter)))

(def operation-to-function
  "
  명령 타입 값과 처리 함수를 연결해주는 해시맵
  "
  {:jmp execute-jmp
   :acc execute-acc
   :nop execute-nop})

(defn execute-operations
  "
  input: 상태값을 나타내는 hashmap
  {:operations input의 명령 벡터
   :accumulator 현재 누산기에 쌓인 값
   :program-counter 현재 실행 중인 명령줄
   :execution-history 여태까지 실행한 명령들)
  output: input의 :program-counter에 해당하는 명령을 실행하고 난 후의 hashmap
  "
  [{:keys [operations
           program-counter]
    :as state}]
  (let [current-operation (nth operations program-counter nil)
        operation (:operation current-operation)]
    (when
      (some? current-operation)
      ((operation-to-function operation) state))))



(defn run-program
  "
  명령 벡터로부터 각 명령 실행 후의 상태에 대한 lazy-seq을 얻는 함수
  input: 프로그램 내 명령들 {:operation 명령 :argument 명령값}의 벡터
  output: {:operation 명령 :argument 명령값}의 lazy-seq
  "
  [operations]
  (->>
    operations
    init-program
    (iterate execute-operations)))

(defn loop?
  "
  input: 명령 실행 전과 후 각각의 상태
  output: 프로그램이 무한루프에 빠졌는지 여부
  "
  [old-state new-state]
  ((:execution-history old-state) (:program-counter new-state)))

(defn terminated?
  "
  input: 명령 실행 전과 후 각각의 상태
  output: 프로그램이 종료되었는지 여부
  "
  [_ new-state]
  (nil? new-state))

(defn stop-execution?
  "
  input: 명령 실행 전과 후 각각의 상태
  output: 프로그램 실행을 더 평가하지 않아도 되는지 여부
  "
  [old-state new-state]
  (or (loop? old-state new-state)
      (terminated? old-state new-state)))

(defn jmp-or-nop?
  "
  input: 명령 실행 전과 후 각각의 상태
  output: 프로그램 실행을 더 평가하지 않아도 되는지 여부
  "
  [operation]
  (#{:jmp :nop} operation))
(comment
  (jmp-or-nop? "acc")
  (jmp-or-nop? :jmp)
  (jmp-or-nop? :nop))

(defn program-counters-of-jmp-and-nop
  "
  input: 프로그램 내 명령들 {:operation 명령 :argument 명령값}의 벡터
  output: jmp, nop 명령들의 program counter의 리스트
  "
  [operations]
  (keep-indexed
    (fn [index {:keys [operation]}] (when (jmp-or-nop? operation) index))
    operations))

(defn switch-jmp-and-nop
  "
  jmp -> nop
  nop -> jmp
  위의 변환을 해주는 함수
  "
  [operation]
  ({:jmp :nop
    :nop :jmp}
   operation))
(comment
  (switch-jmp-and-nop :jmp)
  (switch-jmp-and-nop :nop))

;; reduce 대신 drop-while/first?
(defn accumulator-when-execution-meets-condition
  "
  프로그램 실행을 그치는 조건과 실행 결과의 lazy seq을 받아서 프로그램 실행 상태가 조건을 만족할 시
  accumulator의 값을 꺼내주는 함수
  condition?에 loop?을 넣으면 loop이 발생하기 직전 accumulator값을 꺼내줌
  condition?에 terminated?을 넣으면 종료하고 난 후 accumulator값을 꺼내줌
  input: 두 상태 hashmap을 받아서 boolean을 리턴하는 함수, 명령의 벡터
  output: accumulator 숫자값
  "
  [condition? executions]
  (->> executions
       run-program
       (reduce (fn [old-state new-state]
                 (if (stop-execution? old-state new-state)
                   (reduced (when
                              (condition? old-state new-state)
                              (:accumulator old-state)))
                   new-state)))))


;; part1
(defn accumulator-value-when-loop-detected
  [input-lines]
  (->> input-lines
       (mapv parse)
       (accumulator-when-execution-meets-condition loop?)))
(comment
  (accumulator-value-when-loop-detected input-lines))

;; part2
(defn accumulator-value-after-program-termination
  [input-lines]
  (let [operations (mapv parse input-lines)]
    (->> operations
         program-counters-of-jmp-and-nop
         (map #(update-in operations [% :operation] switch-jmp-and-nop))
         (keep #(accumulator-when-execution-meets-condition terminated? %))
         first)))


(comment
  (accumulator-value-after-program-termination input-lines))

