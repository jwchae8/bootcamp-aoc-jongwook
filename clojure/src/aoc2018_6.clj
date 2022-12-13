(ns aoc2018_6)

(def input-lines (->
                   "resources/day6"
                   slurp
                   clojure.string/split-lines))

(def input-pattern #"(\d+), (\d+)")
(defn parse
  [index input-line]
  (->> input-line
       (re-find input-pattern)
       (drop 1)
       (map parse-long)
       ((fn [[x y]] {:x x
                     :y y
                     :id index}))))
(defn manhattan-distance
  [{point1-x :x point1-y :y}
   {point2-x :x point2-y :y}]
  (+ (abs (- point1-x point2-x))
     (abs (- point1-y point2-y))))

(defn area-in-interest
  [locations]
  (let [xs (map :x locations)
        ys (map :y locations)]
    {:min-x (apply min xs)
     :min-y (apply min ys)
     :max-x (apply max xs)
     :max-y (apply max ys)}))

(defn all-points-in-given-area
  [{:keys [min-x min-y max-x max-y]}]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
    {:x x
     :y y}))

(defn closest-locations-to-point
  [locations point]
  (->> locations
       (map (fn [location] (conj location
                                 {:distance (manhattan-distance location point)})))
       (group-by :distance)
       (apply min-key key)
       val
       (map :id)))

(defn count-as-closest?
  [{:keys [closest-locations]}]
  (= (count closest-locations)
     1))

(defn on-the-border?
  [{:keys [min-x min-y max-x max-y]}
   {:keys [x y]}]
  (or (= min-x x)
      (= max-x x)
      (= min-y y)
      (= max-y y)))

(defn sum-distances
  [locations point]
  (->> locations
       (map (fn [location] (manhattan-distance location point)))
       (reduce +)))







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
  (->> (all-points-in-given-area area)
       (map (fn [point] {:point point
                         :closest-locations (closest-locations-to-point locations point)}))
       (filter count-as-closest?)
       (group-by :closest-locations)
       (map (fn [location] (map :point (val location))))
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

(let [locations (map-indexed parse input-lines)
      area (area-in-interest locations)]
  (->> (all-points-in-given-area area)
       (map (fn [point] {:point point
                         :distance-sum (sum-distances locations point)}))
       (filter (fn [{:keys [distance-sum]}] (<= distance-sum 10000)))
       count))
