(ns miner.pbsim)


(defn init-score []
  {:a 0 :ra 0 :b 0 :rb 0 :side :a :server 2})

(defn finished? [score]
  (or (and (>= (:a score) 11) (>= (:a score) (+ 2 (:b score))))
      (and (>= (:b score) 11) (>= (:b score) (+ 2 (:a score))))))

(defn report-score [score-map]
  (if (= (:side score-map) :a)
    [(:a score-map) (:b score-map) (:server score-map)]
    [(:b score-map) (:a score-map) (:server score-map)]))

(defn opposite-side [side]
  (case side
    :a :b
    :b :a))

(defn next-server [^long server]
  (case server
    1 2
    2 -1))

(defn rally-key [team]
  (case team
    :a :ra
    :b :rb))

(defn update-score [prev-score rally-winner]
  (let [serving (:side prev-score)
        score1 (update prev-score (rally-key rally-winner) inc)]
    (if (= rally-winner serving)
      (update score1 serving inc)
      (if (= (:server score1) 1)
        (assoc score1 :server 2)
        (assoc score1 :side (opposite-side serving) :server 1)))))

(defn percent [pc winner]
  (if (> (rand-int 100) pc)
    (opposite-side winner)
    winner))

(defn pbsim
  ([] (pbsim 40))
  ([percent-server-win]
   (loop [score (init-score)]
     ;;(println (report-score score) score)
     (if (finished? score)
       score
       (recur (update-score score (percent percent-server-win (:side score))))))))


(defn winner [score]
  (if (and (>= (:a score) 11) (>= (:a score) (+ 2 (:b score))))
    :a
    (if (and (>= (:b score) 11) (>= (:b score) (+ 2 (:a score))))
      :b
      nil)))

(defn rally-winner [score]
  (if (> (:ra score) (:rb score))
    :a
    (if (> (:rb score) (:ra score))
      :b
      nil)))

(defn rally-total [score]
  (+ (:ra score) (:rb score)))

(defn point-diff [score]
  (let [a (:a score)
        b (:b score)]
    (if (> a b)
      (- a b)
      (- b a))))

(defn disputed? [score]
  (not= (rally-winner score) (winner score)))


;; avg rally for percent-server-win
;; pbsim 40 -- 43
;; pbsim 45 -- 38
;; pbsim 50 -- 34
;; pbsim 55 -- 21
(defn rally-avg
  ([] (rally-avg 40))
  ([percent-server-win] (rally-avg percent-server-win 1000))
  ([percent-server-win trials]
   (/ (reduce + (map rally-total (repeatedly trials #(pbsim percent-server-win))))
               (double trials))))


(defn diff-avg
  ([] (diff-avg 40))
  ([percent-server-win] (diff-avg percent-server-win 1000))
  ([percent-server-win trials]
   (/ (reduce + (map point-diff (repeatedly trials #(pbsim percent-server-win))))
               (double trials))))
