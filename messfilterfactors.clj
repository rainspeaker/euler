(require '[clojure.core.reducers :as r])

(defn naive-filter [pred xs]
  (reverse 
   (reduce 
    (fn [new-xs x]
      (if (pred x)
        (conj new-xs x)
        new-xs))
    '()
    xs)))

(defn robbie-take-factors [n]
  (r/filter #(zero? (mod n %))
            (into [] (range 2 (Math/sqrt n)))))

(defn robbie-sum-factors [n]
  (+ 1 (r/fold + #(+ %1 %2 (/ n %2)) (robbie-take-factors n))))

(defn naive-take-factors [n]
  (filter #(zero? (mod n %)) (range 1 n)))

(defn naive-sum-factors [n]
  (reduce + (naive-take-factors n)))

(defn sanjay-sum-factors [n]
  (reduce
    #(if (zero? (mod n %2))
      (+ %1 %2 (/ n %2))
      %1)
    1
    (range 2 (Math/sqrt n))))


(defn robbie-sum-factors-2 [n]
  (+ 1 (r/fold 
        (fn ([] 0)
           ([s x] (+ s x (if-not (= (/ n x) x) (/ n x) 0))))
        (r/filter #(zero? (mod n %))
                  (range 2 (inc (Math/sqrt n)))))))

(defn robbie-sum-factors-2-s [n]
  (+ 1 (r/fold 
        (fn ([] 0)
           ([s x] (+ s x (if-not (= (/ n x) x) (/ n x) 0))))
        (filter #(zero? (mod n %))
                  (range 2 (inc (Math/sqrt n)))))))



(defn robbie-sum-factors-3 [n]
  (reduce 
   (fn [s x] 
     (let [x' (/ n x)]
       (+ s x (if (= x' x) 0 x'))))
   1
   (robbie-take-filters-3 n)))

(defn robbie-sum-factors-3 [n]
  (reduce 
   (fn [s x] 
     (let [x' (/ n x)]                    
       (+ s x (if (= x' x) 0 x'))))        
   1                                       
   (filter #(zero? (mod n %))
           (range 2 (inc (Math/sqrt n))))))

(defn robbie-sum-factors-3 [n]
  (->> (range 2 (inc (Math/sqrt n)))
       (filter #(zero? (mod n %)))
       (reduce (fn [s x]
                 (let [x' (/ n x)]
                   (+ s x (if (= x' x) 0 x')))))
       (+ 1)))


(defn robbie-sum-factors-2-r [n]
  (+ 1 (reduce
        (fn ([] 0)
          ([s x] (+ s x (if-not (= (/ n x) x) (/ n x) 0))))
        (r/filter #(zero? (mod n %))
                  (range 2 (inc (Math/sqrt n)))))))

(defn sanjay-take-factors-2 [n]
  (reduce
    (fn [a b]
      (conj a b (/ n b)))
    #{1}
    (filter
      #(zero? (mod n %))
      (range 2 (inc (Math/sqrt n))))))

(defn sanjay-sum-factors-2 [n]
  (reduce + 1 (sanjay-take-factors-2 n)))

(defn sanjay-take-factors-2-s [n]
  (reduce
    (fn [a b]
      (conj a b (/ n b)))
    #{1}
    (filter
      #(zero? (mod n %))
      (range 2 (inc (Math/sqrt n))))))

(defn sanjay-sum-factors-2-s [n]
  (reduce + 1 (seq (sanjay-take-factors-2-s n))))

(defn sanjay-take-factors-2r1 [n]
  (reduce
    (fn [a b]
      (conj a b (/ n b)))
    #{1}
    (filter
      #(zero? (mod n %))
      (range 2 (inc (Math/sqrt n))))))

(defn sanjay-sum-factors-2r1 [n]
  (+ 1 (r/fold + (sanjay-take-factors-2r1 n))))

(defn sanjay-take-factors-2r2 [n]
  (r/fold
   clojure.set/union 
   (fn [a b]
      (conj a b (/ n b)))
   (r/filter #(zero? (mod n %))
             (into [] (range 2 (inc (Math/sqrt n)))))))

(defn sanjay-sum-factors-2r2 [n]
  (+ 1 (r/fold + (sanjay-take-factors-2r2 n))))
