(ns aoc2018-1)

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(defn sum [x] (reduce + x))

(comment
  (sum [1 2 3]))
;; apply 함수 사용 시도해보기
(comment (
           apply + [1 2 3]))

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(defn find-first-repeated-occurrence [x]
  (reduce (fn [visited val]
            (if (visited val) (reduced val) (conj visited val)))
          #{0}
          x))

(comment (find-first-repeated-occurrence (reductions + (cycle [3 3 4 -2 -4]))))

;; 데이터를 가공하는 함수를 별도로 만들거나 let을 이용해보기 데이터와 로직 분리하기
;; threading macro 사용하기
(max [1234])
(->> [3 3 4 -2 -4]
     cycle
     (reductions +)
     find-first-repeated-occurrence
     ,)
