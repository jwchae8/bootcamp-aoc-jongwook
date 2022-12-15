(ns aoc2018_6)

;; example
;; 165, 169
(def input-lines (->
                   "resources/day6"
                   slurp
                   clojure.string/split-lines))

;; 파싱을 위한 정규표현식!
(def input-pattern #"(\d+), (\d+)")
(defn parse
  "
  map-indexed의 인자가 되는 파싱함수.
  input: input의 번째 수 및 값
  output: {:x x좌표, :y y좌표, :id 번째수}
  example
  0 \"165, 169\" -> {:x 165 :y 169 :id 0}
  "
  [index input-line]
  (->> input-line
       (re-find input-pattern)
       (drop 1)
       (map parse-long)
       ((fn [[x y]] {:x x
                     :y y
                     :id index}))))
(parse 0 "165, 169")

(defn manhattan-distance
  "
  두 위치 사이의 manhattan distance를 구하는 함수
  input: 두 위치({:x x좌표, :y y좌표, :id 번째수})
  output: 거리
  example
  {:x 0, :y 0, :id 0} {:x 1, :y 1, :id 1} -> 2
  "
  [{point1-x :x point1-y :y}
   {point2-x :x point2-y :y}]
  (+ (abs (- point1-x point2-x))
     (abs (- point1-y point2-y))))
(manhattan-distance {:x 0 :y 0 :id 0} {:x 1 :y 1 :id 1})

(defn area-in-interest
  "
  문제 1, 2 모두 주어진 좌표를 모두 포함하는 최소 영역만 고려하면 되기 때문에
  x, y 최대 최소 좌표를 가지는 hashmap을 구하는 함수
  input: 주어진 좌표들({:x x좌표, :y y좌표, :id 번째수})
  output: {:min-x x좌표 중 최소값 max-x x좌표 중 최대값 :min-y y좌표 중 최소값 :max-y y좌표 중 최대값}
  example
  {:x 0, :y 0, :id 0} {:x 1, :y 1, :id 1} -> {:min-x 0 :min-y 0 :max-x 1 :max-y 1}
  "
  [locations]
  (let [xs (map :x locations)
        ys (map :y locations)]
    {:min-x (apply min xs)
     :min-y (apply min ys)
     :max-x (apply max xs)
     :max-y (apply max ys)}))
(area-in-interest [{:x 0, :y 0, :id 0} {:x 1, :y 1, :id 1}])
(defn all-points-in-given-area
  "
  주어진 영역에 있는 모든 점의 좌표들을 구하는 함수
  input: 영역 {:min-x x좌표 중 최소값 max-x x좌표 중 최대값 :min-y y좌표 중 최소값 :max-y y좌표 중 최대값}
  output: {:x x좌표 :y y좌표}의 리스트
  example
  {:min-x 0 :max-x 1 :min-y 0 :max-y 1} -> ({:x 0, :y 0} {:x 0, :y 1} {:x 1, :y 0} {:x 1, :y 1})
  "
  [{:keys [min-x min-y max-x max-y]}]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
    {:x x
     :y y}))
(all-points-in-given-area {:min-x 0 :max-x 1 :min-y 0 :max-y 1})

(defn closest-locations-to-point
  "
  입력 상에 주어진 위치들 중 주어진 점과 가장 가까운 곳들의 인덱스를 구하는 함수
  input: locations - '({:id 번째수 :x x좌표 :y y좌표} ...) point - {:x x좌표 :y y좌표}
  output: 가장 가까운 위치의 id들의 리스트
  example
  '({:id 0 :x 0 :y 0} {:id 1 :x 5 :y 5}) {:x 1 :y 1} -> '(0)
  "
  [locations point]
  (->> locations
       (map (fn [location] (conj location ;; assoc를 사용하기
                                 {:distance (manhattan-distance location point)})))
       (group-by :distance)
       (apply min-key key)
       val
       (map :id)))
(closest-locations-to-point '({:id 0 :x 0 :y 0} {:id 1 :x 5 :y 5}) {:x 1 :y 1})
(closest-locations-to-point '({:id 0 :x 0 :y 0} {:id 1 :x 4 :y 4}) {:x 2 :y 2})

(defn count-as-closest?
  "
  특정 좌표로부터 가장 가까운 위치로 셀 지 여부를 구하는 함수
  input: 가장 가까운 위치의 리스트
  output: 가장 가까운 위치가 갯수가 하나인지 여부
  "
  [{:keys [closest-locations]}]
  (= (count closest-locations)
     1))

(defn on-the-border?
  "
  주어진 위치가 영역의 경계선 상에 있는지 여부
  input: {:min-x x좌표 중 최소값 max-x x좌표 중 최대값 :min-y y좌표 중 최소값 :max-y y좌표 중 최대값}
         {:x x좌표 :y y좌표}
  output: 경계선상에 있는지 여부
  {:min-x 0 :min-y 0 :max-x 3 :max-y 3} {:x 1 :y 1} -> false
  {:min-x 0 :min-y 0 :max-x 3 :max-y 3} {:x 1 :y 3} -> true
  "
  [{:keys [min-x min-y max-x max-y]}
   {:keys [x y]}]
  (or (= min-x x)
      (= max-x x)
      (= min-y y)
      (= max-y y)))
(on-the-border? {:min-x 0 :min-y 0 :max-x 3 :max-y 3} {:x 1 :y 1})
(on-the-border? {:min-x 0 :min-y 0 :max-x 3 :max-y 3} {:x 1 :y 3})

(defn sum-distances
  "
  주어진 점과 입력 상에 주어진 위치 각각의 거리의 합
  input: locations - '({:id 번째수 :x x좌표 :y y좌표} ...) point - {:x x좌표 :y y좌표}
  output: 거리의 합
  example
  '({:id 0 :x 0 :y 0} {:id 1 :x 5 :y 5}) {:x 1 :y 1} -> 10
  "
  [locations point]
  (->> locations
       (map (fn [location] (manhattan-distance location point)))
       (reduce +)))

(sum-distances '({:id 0 :x 0 :y 0} {:id 1 :x 5 :y 5}) {:x 1 :y 1})





;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(let [locations (map-indexed parse input-lines)
      area (area-in-interest locations)
      on-the-border-of-the-area? (partial on-the-border? area)]
  (->> area
       all-points-in-given-area
       (map (fn [point] {:point point
                         :closest-locations (closest-locations-to-point locations point)}))
       (filter count-as-closest?)
       (group-by :closest-locations)
       ;; vals
       ;; (map :point)
       (map (fn [location] (map :point (val location)))) ;; location 더 알맞은 이름으로 map-vals
       (remove (fn [points] (some on-the-border-of-the-area? points)))
       (map count)
       (apply max)))

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.
;; 최종 풀이도 함수화하자
(comment (let [locations (map-indexed parse input-lines)]
           (->> locations
                area-in-interest
                all-points-in-given-area
                (map (fn [point] (sum-distances locations point)))
                (filter #(< % 10000))
                count)))

