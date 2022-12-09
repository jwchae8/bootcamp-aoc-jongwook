(ns aoc2018_4)

;; example
;; [1518-07-04 00:01] falls asleep
;; [1518-06-27 00:42] wakes up
;; [1518-06-07 00:03] Guard #2789 begins shift
(def input-lines (->
                   "resources/day4"
                   slurp
                   clojure.string/split-lines))

;; 파싱을 위한 정규표현식
;; 분(minute)값과 가드의 ID만을 구한다
(def input-pattern #":(\d+)[^0-9]*(\d+)?")

;; nil이면 nil, 아니면 parse-long 결과를 반환
;; 이게 idiomatic한 접근인지는 잘 모르겠습니다만...
;; example
;; "1234" -> 1234
;; nil -> nil
(defn parse-long-or-nil [string] (if (nil? string)
                                   nil
                                   (parse-long string)))
(parse-long-or-nil "12345")
(parse-long-or-nil nil)

;; input: 로그 한 줄
;; output: {:minute 분, :guard-id 가드 ID 혹은 nil}
;; example
;; [1518-07-04 00:01] falls asleep -> {:minute 1, :guard-id nil}
;; [1518-06-07 00:03] Guard #2789 begins shift -> {:minute 3, :guard-id 2789}
(defn parse-log [log] (->>
                        log
                        (re-find input-pattern)
                        rest
                        (map parse-long-or-nil)
                        (zipmap [:minute :guard-id])))
(parse-log "[1518-07-04 00:01] falls asleep")
(parse-log "[1518-06-27 00:42] wakes up")
(parse-log "[1518-06-07 00:03] Guard #2789 begins shift")


;; input: {:minute 분값 :guard-id 가드ID 혹은 nil값}으로 이뤄진 시퀀스
;; output: {:logs 한 가드가 자거나 깬 분값의 시퀀스 :guard-id 가드ID}로 이뤄진 시퀀스
(defn group-logs-by-guard-id [logs] (->>
                                      logs
                                      (partition-by (fn [log] (nil? (:guard-id log))))
                                      (partition 2)
                                      (map (fn [[guard minutes]]
                                             {(:guard-id (last guard)) (map :minute minutes)}))
                                      (apply merge-with concat)
                                      (map (fn [[guard minutes]]
                                             {:guard-id guard
                                              :logs (map (fn [interval]
                                                           (zipmap [:sleep :wake] interval))
                                                         (partition 2 minutes))}))))

(->>
  input-lines
  sort
  (map parse-log)
  group-logs-by-guard-id)

;; input: {:logs 한 가드가 자거나 깬 분값의 시퀀스 :guard-id 가드ID}
;; output: 해당 가드가 잔 총 시간
;; example
;; {:guard-id 1 :logs [{:wake 10 :sleep 5} {:wake 20 :sleep 15}]} -> 10
(defn sleep-time
  [{:keys [logs]}]
  (->>
   logs
   (map (fn [{:keys [wake sleep]}] (- wake sleep)))
   (apply +)))
(sleep-time {:guard-id 1 :logs [{:wake 10 :sleep 5} {:wake 20 :sleep 15}]})

;; input: {:logs 한 가드가 자거나 깬 분값의 시퀀스 :guard-id 가드ID}의 시퀀스
;; output: 가장 오래 잔 가드의 ID
(defn max-sleep-time-guard
  [log-by-guards]
  (->>
    log-by-guards
    (apply max-key sleep-time)))

;; input: {:logs 한 가드가 자거나 깬 분값의 시퀀스 :guard-id 가드ID}
;; output: {:guard-id 가드ID
;;          :minute 가장 많이 잔 분
;;          :frequency 잔 횟수}
(defn most-sleeping-minute
  [{:keys [guard-id logs]}]
  (->>
    logs
    (map (fn [{:keys [sleep wake]}] (range sleep wake)))
    (reduce concat)
    frequencies
    (apply max-key val)
    ((fn [[minute frequency]]
       {:guard-id guard-id
        :minute minute
        :frequency frequency}))))


(defn answer [{:keys [guard-id minute]}] (* guard-id minute))





;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

(->>
  input-lines
  sort
  (map parse-log)
  group-logs-by-guard-id
  max-sleep-time-guard
  most-sleeping-minute
  answer)

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(->>
  input-lines
  sort
  (map parse-log)
  group-logs-by-guard-id
  (map most-sleeping-minute)
  (apply max-key :frequency)
  answer)