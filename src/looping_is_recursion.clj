(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [a n k]
                         (if (zero? k)
                           a
                           (recur (* a n) n (dec k))))]
    (power-helper 1 base exp)))

(defn last-element [a-seq]
  (let [last-element-helper (fn [a s]
                                (if (empty? s)
                                  a
                                  (recur (first s) (rest s))))]
    (last-element-helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [seq1-empty (empty? seq1)
        seq2-empty (empty? seq2)
        seq1-head  (first seq1)
        seq1-tail  (rest seq1)
        seq2-head  (first seq2)
        seq2-tail  (rest seq2)]
  (cond
    (and seq1-empty
         seq2-empty) true
    seq1-empty       false
    seq2-empty       false
    (not= seq1-head
          seq2-head) false
    :else            (recur seq1-tail seq2-tail))))

(defn find-first-index [pred a-seq]
  (loop [curr  0
         a-seq a-seq]
        (cond
          (empty? a-seq)       nil
          (pred (first a-seq)) curr
          :else                (recur (inc curr) (rest a-seq)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    0
    (loop [total        0
           num-elements 0
           a-seq        a-seq]
          (if (empty? a-seq)
            (/ total num-elements)
            (recur (+ total (first a-seq))
                   (inc num-elements)
                   (rest a-seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-seq a-seq
         par   #{}]
        (if (empty? a-seq)
          par
          (recur (rest a-seq)
                 (toggle par (first a-seq))))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         n n]
        (if (zero? n)
          a
          (recur b (+ a b) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq      a-seq
         no-rep-set #{}
         no-rep     []]
        (cond
          (empty? a-seq)                       no-rep
          (contains? no-rep-set (first a-seq)) no-rep
          :else                                (recur (rest a-seq)
                                                      (conj no-rep-set
                                                            (first a-seq))
                                                      (conj no-rep
                                                            (first a-seq))))))
