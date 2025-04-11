;; RUN FILE:
;;           clojure bernoulli.clj

; Func for binom which takes parameters n and k
(defn binom [n k]
  (let [r (atom 1.0)] ; define r = 1 as atom (muteable)
    ; loop in range 1 to k inclusive
    (doseq [i (range 1 (inc k))]
      ; swap changes atom-value with func value
      ; the func 'fn' takes current=r and switches r to r · (n − i + 1)/i 
      (swap! r (fn [current] (* current (/ (+ (- n i) 1) i)))))  
    @r) ; @r dereferences variable r and returns r
)

; func that takes in n and returns the nth bernoulli number
(defn B [n]
  (let [B (atom (vec (repeat (inc n) 0.0)))] ; muteable vector B with n st zeroes (n inclusive)
    (swap! B assoc 0 1.0) ; swap to modify B, assoc to change value of B(0)=1.0
    ; loop m from 1 to n (inclusive)
    (doseq [m (range 1 (inc n))]
      (swap! B assoc m 0.0) ; B[m] = 0
      ; loop k from 0 to m-1 (m not inclusive)
      (doseq [k (range 0 m)]
        ; swap updates the vector stored in B
        ; fn for x=B performs assoc on vector x=B and stores the calculation at index m 
        (swap! B (fn [x] (assoc x m (- (get x m) (* (binom (inc m) k) (get x k)))))) ; calculate
        )
      ; update -> modify value (assoc = change value entirely)
      ; double -> converts x to double type for correct division
      (swap! B update m (fn [x] (/ (double x) (+ 1 m)))) ; divide with m+1
      )
    (get @B n)) ; retrieve output B[n]
  )

;; (println (binom 5 2))
;; (println (B 4)) ; should return -0.03333... = (-1/30)

; Print the first 10 bernoulli numbers -> 'B(n)=B[n]'
(doseq [n (range 10)]
  (println (format "B(%d) = %.6f" n (B n))) ; print 6 decimals
  )