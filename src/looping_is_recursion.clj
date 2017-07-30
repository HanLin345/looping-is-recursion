(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [y]
                 (if (<= (count y) 1)
                   (first y)
                   (recur (rest y))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (seq= (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         f pred
         n a-seq]
    (if (empty? n)
      nil
      (if (f (first n))
        acc
        (recur (+ acc 1) f (rest n))))))
(defn avg [a-seq]
  (loop [acc 0
         counter 0
         n a-seq]
    (if (empty? n)
      (/ acc counter)
      (recur (+ acc (first n)) (+ counter 1) (rest n)))))

(defn parity [a-seq]
  (loop [n a-seq
        result #{}]
    (if(not (empty? n))
      (if(contains? result (first n))
        (recur (rest n) (disj result (first n)))
        (recur (rest n) (conj result (first n))))
      result)))


(defn fast-fibo [n]
  (loop [f0 0
         f1 1
         counter n]
    (if (= 0 counter)
      f0
      (recur f1 (+ f0 f1) (dec counter)))))

(defn cut-at-repetition [a-seq]
  (loop [x a-seq
         y []
         z #{}]
       (if (or (empty? x) (contains? z (first x)))
         y
         (recur (rest x) (conj y (first x)) (conj z (first x))))))

