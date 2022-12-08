(ns aoc2018_3)

;; example
;; #1 @ 669,271: 17x11
(def input-lines (->
                   "resources/day3_part1"
                   slurp
                   clojure.string/split-lines))
(def input-lines2 (->
                    "resources/day3_part2"
                    slurp
                    clojure.string/split-lines))

;; 문제 input 파싱을 위한 regex 패턴
(def input-pattern #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
;; input: input 파일 내 input 한 줄
;; output: 숫자 벡터(사각형 id, 좌표, 크기 등)
;; example
;; #1 @ 669,271: 17x11 -> (1 669 271 17 11)
(defn parse-input [input] (->>
                            input
                            (re-find input-pattern)
                            rest
                            (map parse-long)))


;; input: regex 패턴을 통해 파싱된 input값
;; output: 리스트 값들에 key를 붙여서 만든 hashmap
;; example
;; (1 2 3 4 5) -> {:id 1 :x-coord 2 :y-coord 3 :width 4 :height 5}
(defn build-hashmap-from-input [[id x-coord y-coord width height]]
  {
   :id id
   :x-coord x-coord
   :y-coord y-coord
   :width width
   :height height})

;; input: :id, :x-coord, :y-coord, :width, :height로 구성된 hashmap
;; output: 해당 사각형이 위치하고 있는 좌표들의 벡터
;; example
;; {:id 1 :x-coord 2 :y-coord 2 :width 2 :height 2} -> [[2 2] [2 3] [3 2] [3 3]]
(defn build-coordinate-vector-from-hashmap
  [{:keys [x-coord y-coord width height]}]
  (for [x (range x-coord (+ x-coord width))
        y (range y-coord (+ y-coord height))]
    [x y]))

;; input: :id, :x-coord, :y-coord, :width, :height로 구성된 hashmap
;; output: 해당 사각형이 위치하고 있는 좌표들을 키로 하고 :id를 value로 가지는 hashmap
;; example
;; {:id 1 :x-coord 2 :y-coord 2 :width 2 :height 2} -> {[2 2] 1, [2 3] 1, [3 2] 1, [3 3] 1}
(defn build-coordinate-to-id-set-hashmap
  [rect]
  (zipmap (build-coordinate-vector-from-hashmap rect) (repeat #{(:id rect)})))

;; input: 사각형 id들이 든 set
;; output: id를 key로 가지고 set의 크기가 value인 hashmap
;; example
;; #{1 2 3 4} -> {1 4, 2 4, 3 4, 4 4}
(defn build-id-to-count-hashmap [id-set] (zipmap id-set (repeat (count id-set))))

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(comment (->>
           input-lines
           (map parse-input)
           (map build-hashmap-from-input)
           (map build-coordinate-vector-from-hashmap)
           (map frequencies)
           (reduce (fn [x y] (merge-with + x y)))
           vals
           (filter #(> % 1))
           count))


;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(comment (->>
           input-lines2
           (map parse-input)
           (map build-hashmap-from-input)
           (map build-coordinate-to-id-set-hashmap)
           (reduce (fn [x y] (merge-with into x y)))
           vals
           (map build-id-to-count-hashmap)
           (reduce (fn [x y] (merge-with max x y)))
           (reduce (fn [x y] (if (= (val y) 1)
                               (reduced (key y))
                               y)))))
