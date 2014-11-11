(ns looping-is-recursion)

(defn power [base exp]
  (let [helper
        (fn [acc base exp]
          (if (zero? exp)
            acc
            (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper
        (fn [elem a-seq]
          (if (empty? a-seq)
            elem
            (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (empty? seq1) false
    (empty? seq2) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         rest-seq a-seq]
    (cond
      (empty? rest-seq) nil
      (pred (first rest-seq)) idx
      :else (recur (inc idx) (rest rest-seq)))))

(defn avg [a-seq]
  (loop [cnt 0
         sum 0
         rest-seq a-seq]
    (cond
      (and (== cnt 0) (empty? rest-seq)) 0
      (empty? rest-seq) (/ sum cnt)
      :else (recur (inc cnt)
                   (+ sum (first rest-seq))
                   (rest rest-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [s #{}
         rest-seq a-seq]
    (if (empty? rest-seq)
      s
      (recur (toggle s (first rest-seq))
             (rest rest-seq)))))

(defn fast-fibo [n]
  (loop [sum1 1
         sum2 0
         r n]
    (cond
      (== r 0) 0
      (== r 1) 1
      (== r 2) (+' sum1 sum2)
      :else (recur (+' sum1 sum2) sum1 (dec r)))))

(defn cut-at-repetition [a-seq]
  (loop [s #{}
         build []
         rest-seq a-seq]
    (if (empty? rest-seq)
      build
      (let [f (first rest-seq)
            r (rest rest-seq)]
        (if (contains? s f)
          build
          (recur (conj s f) (conj build f) r))))))

