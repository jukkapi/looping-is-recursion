(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (cond
                   (zero? k) 1
                   (zero? (dec k)) (* acc n)
                   :else (recur (* n acc) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (not (second a-seq)) (first a-seq)
    (recur (rest a-seq))))


(defn seq= [seq1 seq2]
  (loop [e1 seq1
         e2 seq2]
  (cond (and (empty? e1) (not (empty? e2))) false
        (and (empty? e2) (not (empty? e1))) false
        (not (= (first e1) (first e2))) false
        (and (empty? e1) (empty? e2)) true
        :else (recur (rest e1) (rest e2)))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         s a-seq]
    (cond (nil? (first s)) nil
          (pred (first s)) n
          :else (recur (inc n) (rest s)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         s a-seq]
    (if (empty? s)
          (/ sum n)
          (recur (+ sum (first s)) (inc n) (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         s a-seq]
    (if (empty? s) acc
      (recur (toggle acc (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [ idx 0
          prev 1
          curr 0]
    (cond (zero? n) 0
          (= n 1) 1
          (= n 2) 1
          (= n idx) curr
          :else (recur (inc idx) curr (+ curr prev)))))

(defn cut-at-repetition [a-seq]
  (loop [rest-seq a-seq
         acc-list []
         acc-set #{}]
      (cond (empty? rest-seq) acc-list
            (contains? acc-set (first rest-seq)) acc-list
            :else (recur (rest rest-seq) (conj acc-list (first rest-seq)) (conj acc-set (first rest-seq))))))


