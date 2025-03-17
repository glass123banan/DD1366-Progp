;; RUN FILE:
;;           clojure bernoulli.clj

(defn binom [n k]
  (let [r (atom 1)]
    ; loop in range 1 to k inclusive
    (doseq [i (range 1 (inc k))]
      ; swap changes atom-value with func value
      (swap! r (fn [current] (* current (/ (+ (- n i) 1) i)))))  ; current = r, divide () with i
    @r) ; @r dereferences variable r and returns r
)

;; (defn B [n]
;;   (let [B (make-array Float/TYPE n)]
;;   (aset B 0 (atom 1)) ; set B[0] = 1
;;   (doseq [m (range 1 (inc n))]
;;     (aset B m (atom 0)) 
;;     (doseq [k (range 0 (inc (- m 1)))]
;;       (swap! (aget B m) (fn [current] (- current (* (binom (+ m 1) k) (aget B k)))))
;;       )
;;     (swap! (aget B m) (fn [current] (/ current (+ m 1))))
;;     )
;;   (swap! (aget B (dec n)) identity)  ;; Dereference the atom and return the value
;;     @(aget B (dec n))))

;; (defn B [n]
;;   (let [B (make-array Float/TYPE n)]
;;     (aset B 0 1)
;;     (doseq [m (range 1 (inc n))]
;;       (aset B m 0) ; set B[m] = 0
;;       (doseq [k (range 0 (inc (- m 1)))]
;;         (aset B m ( (fn [current] (- current (* (binom (+ m 1) k) (aget B k))))))
;;       )
;;       (aset B m (fn [current] (/ current (+ m 1))))
;;     ) 
;;     (aget B n)
;;   )
;; )

(defn B [n]
  (let [B (atom (vec (repeat n 0)))] ; allocate B[0]..B[n-1] = zeros
    (swap! B assoc 0 1) ; B[0] = 1
    (doseq [m (range 1 (inc n))] ; 1..n
      (swap! B assoc m 0) ; B[m] = 0
      (doseq [k (range m)] ; 0..(m-1)
        (swap! B #(assoc % m  ; B[m] = ...
                         (-
                          (get % m) ; B[m]
                          (*
                           (binom (inc m) k)
                           (get % k)))))) ; B[k]
      (swap! B update m ; B[m] = B[m] ...
             #(/ % (inc m))))
    (get @B n)))

(println (binom 5 2))
(println (B 1))