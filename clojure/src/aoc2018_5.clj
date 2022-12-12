(ns aoc2018_5)
(require '[clojure.string :as str])

(def input (->
             "resources/day5"
             slurp))

(defn react?
  "
  두 문자가 서로 반응할 수 있는지(동일한 알파벳이면서 case가 서로 다른지) 검사하는 함수
  input: 두 캐릭터
  output: boolean값(두 문자가 서로 반응할 수 있는지)
  example
  a A -> true
  a a -> false
  a b -> false
  "
  [char1 char2]
  (and (not-any? nil? (list char1 char2))
       (= (abs (- (int char1) (int char2))) 32)))
(react? \A nil)
(react? \A \a)
(react? \a \b)
(react? \a \a)

(defn polymer-after-react
  "
  주어진 문자열이 문제에서 정의된 반응을 마치고 난 이후 남은 문자열
  input: 문자열
  output: 반응 이후의 문자열
  example
  bcCAa -> b
  "
  [polymer]
  (reduce
    (fn [stack char]
      (if (react? (peek stack) char)
        (pop stack)
        (conj stack char)))
    [] polymer))
(polymer-after-react "bcCAa")

(defn distinct-elements-in-polymer
  "
  string을 받아서 string을 구성하는 서로 다른 알파벳을 case-insensitive하게 구하는 함수
  input: string
  output: 서로 다른 알파벳(lower-case)의 리스트
  example
  abcde -> '(a b c d e)
  aaaaa -> '(a)
  aAaAa -> '(a)
  "
  [polymer]
  (->
    polymer
    str/lower-case
    distinct))
(distinct-elements-in-polymer "abcde")
(distinct-elements-in-polymer "aAaAa")

(defn remove-elements-in-polymer
  "
  string과 알파벳을 받아 string으로부터 주어진 알파벳을 case-insensitive하게 제거하는 함수
  input: string 및 알파벳
  output: 알파벳이 제거된 string
  example
  abcdA a -> bcd
  "
  [polymer element]
  (remove #{element (first (str/upper-case element))} polymer))
(remove-elements-in-polymer "abcdA" \a)


;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(->
  "dabAcCaCBAcCcaDA"
  polymer-after-react)

;; For AOC answer
(->
  input
  polymer-after-react
  count)

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(->>
  input
  distinct-elements-in-polymer
  (map (fn [element] (remove-elements-in-polymer input element)))
  (map (fn [polymer] (-> polymer
                         polymer-after-react
                         count)))
  (apply min))