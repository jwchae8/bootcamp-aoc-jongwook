(ns aoc2018-2)

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(def input (->
             "resources/day2_part1"
             slurp
             clojure.string/split-lines))

;; 주석으로 input/output 정리
;; has-value-n은 너무 trivial한 함수라서 그냥 호출해도 괜찮을 것 같다
;; boolean을 반환하면 함수 이름 끝에 ?를 붙이는 convention있음
;; 좀 더 명확하게 결과값을 알고 싶으면 contains?를 쓰는 것이 좋음
;(defn has-value-n [n]
;  (fn [coll] (coll n)))
(defn filter-value-n [n]
  (partial filter (fn [count-set] (count-set n))))

;; parameter이름에 coll을 썼지만, vector가 들어가면 검사가 안됨
;(defn has-value-2-3 [coll]
;  ((juxt (has-value-n 2) (has-value-n 3)) coll))

(defn has-value-2-3 [count-set]
  ((juxt #(% 2) #(% 3)) count-set))

(defn accumulate-non-nil [idx]
  (fn [xs] (reduce (fn [x y] (if (nil? (nth y idx))
                               x
                               (+ 1 x))) 0 xs)))

(defn filter-value-2-3 [xs]
  ((juxt (filter-value-n 2) (filter-value-n 3)) xs))

;; mapv는 eager한 함수(cycle과 함께 쓰면 무한 루프에 빠지게 됨)
(defn transpose [m]
  (apply mapv vector m))



(defn counters-set [text] (->
                             text
                             frequencies
                             vals
                             set))
(->
  "abbccc"
  counters-set
  filter-value-2-3)


;;method 1
(->>
  input
  (map counters-set)
  filter-value-2-3
  (map count)
  (reduce *))

;;method 2
(->>
  input
  (map counters-set)
  (map has-value-2-3)
  ((juxt (accumulate-non-nil 0) (accumulate-non-nil 1)))
  (reduce *))
;;method 3
(->>
  input
  (map counters-set)
  (map has-value-2-3)
  transpose
  (map (fn [xs] (remove nil? xs)))
  (map count)
  (reduce *))


;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.

(def input2 (->
              "resources/day2_part2"
              slurp
              clojure.string/split-lines))
(def indexed-input2 (map-indexed vector input2))
(defn str-diff [str1 str2]
  (->>
    (map vector str1 str2)
    (filter #(apply not= %))
    count))
(str-diff "asdf" "assf")
(defn find-common [str1 str2]
  (->>
    (map (fn [x y] (if (= x y)
                     x
                     nil)) str1 str2)
    (remove nil?)
    clojure.string/join))

(find-common "asdf" "assf")
(defn find-common-from-one-char-diff-pair [indexed-xs]
  (->
    (for [[idx1 str1] indexed-xs
          [idx2 str2] indexed-xs
          :while (> idx1 idx2)
          :when (= (str-diff str1 str2) 1)]
      (find-common str1 str2))
    first))

(find-common-from-one-char-diff-pair indexed-input2)

;; #################################
;; ###        Refactoring        ###
;; #################################
