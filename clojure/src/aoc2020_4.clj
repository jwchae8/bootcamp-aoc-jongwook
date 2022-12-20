(ns aoc2020_4
  (:require [clojure.spec.alpha :as s]))

(def input-lines
  "
  example
  eyr:1960
  ecl:oth
  iyr:2017 pid:022131529 cid:79
  hgt:184
  byr:2005 hcl:#6b5442
  "
  (->>
    "resources/aoc2020/day4"
    slurp
    clojure.string/split-lines))

(defmulti parse-field-value
          "
          첫 원소는 필드 이름에 해당하는 키워드, 두 번째 원소는 필드값인 벡터를 받아서
          첫 원소의 값에 따라 행동이 달라지는 multimethod입니다
          "
          first)

(defmethod parse-field-value :byr [[_ byr]]
  "
  [:byr \"2000\"] -> {:byr 2000}
  "
  {:byr (parse-long byr)})
(defmethod parse-field-value :iyr [[_ iyr]]
  "
  [:iyr \"2000\"] -> {:iyr 2000}
  "
  {:iyr (parse-long iyr)})
(defmethod parse-field-value :eyr [[_ eyr]]
  "
  [:eyr \"2000\"] -> {:eyr 2000}
  "
  {:eyr (parse-long eyr)})
(defmethod parse-field-value :hgt [[_ hgt]]
  "
  [:hgt \"200cm\"] -> {:hgt {:num 200 :unit cm}}
  [:hgt \"80in\"] -> {:hgt {:num 80 :unit in}}
  [:hgt \"100\"] -> {:hgt {:num nil :unit nil}}
  "
  {:hgt (->> hgt
             (re-find #"(\d+)(cm|in)")
             ((fn [[_ num unit]] {:num (when num (parse-long num))
                                  :unit unit})))})
(defmethod parse-field-value :default [[field-key field-value]]
  "
  [:ecl \"oth\"] -> {:ecl \"oth\"}
  "
  {field-key field-value})

(defn parse-field
  "
  key:value 형태의 field string에 대한 parsing
  input: field 하나에 해당하는 string
  output: key는 field 이름에 해당하는 keyword이고 value는 field 값인 hashmap
  \"byr:2020\" -> {:byr 2020}
  "
  [field]
  (-> field
      (clojure.string/split #":")
      ((fn [[field-key field-value]]
         [(keyword field-key) field-value]))
      parse-field-value))

(defn parse-passport
  "
  한 여권에 대한 parsing을 처리해주는 함수
  input: 공백으로 구분되는 한 여권에 대한 정보
  output: 여권 정보에 대한 hashmap
  "
  [passport]
  (->> passport
       (mapcat (fn [line] (clojure.string/split line #" ")))
       (map parse-field)
       (apply merge)))
(defn parse
  "
  file 입력에 대한 전체 parsing을 해주는 함수
  "
  [input-lines]
  (->> input-lines
       (partition-by clojure.string/blank?)
       (remove (fn [part] (some clojure.string/blank? part)))
       (map parse-passport)))



(comment
  (defn valid-hgt? [{:keys [num unit]}]
    (case unit
      "cm" (<= 150 num 193)
      "in" (<= 59 num 76)
      false))
  (s/def ::byr (s/int-in 1920 2003))
  (s/def ::iyr (s/int-in 2010 2021))
  (s/def ::eyr (s/int-in 2020 2031))
  (s/def ::hgt valid-hgt?)
  (s/def ::hcl (partial re-matches #"#[a-f0-9]{6}"))
  (s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
  (s/def ::pid (partial re-matches #"[0-9]{9}"))
  (s/def ::cid any?))

(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))

(comment (->> input-lines
              parse
              (filter #(s/valid? ::passport %))
              count))
