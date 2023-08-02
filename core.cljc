(ns easing.core)

(defn almost-identity [x m n]
  (if (> x m)
    x
    (let [a (- (* 2 n)
               m)
          b (- (* 2 m)
               n)
          t (/ x m)]
      (+ (* (+ b (* a t))
            t
            t)
         n))))

(defn exponential-impulse [x k]
  (* k x (Math/exp (- 1 (* k x)))))

(defn cubic-pulse [c w x]
  (let [x (abs (- x c))]
    (if (> x w)
      0
      (let [x3 (/ x w)]
        (- 1 (* x3 x3 (- 3 (* 2 x3))))))))

(defn exponential-step [x k n]
  (Math/exp (* (- k)
               (Math/pow x n))))

(defn gain [x k]
  (let [a (* 0.5
             (Math/pow (* 2
                          (if (< x 0.5)
                            x
                            (- 1 x)))
                       k))]
    (if (< x 0.5)
      a
      (- 1 a))))

(defn parabola [x k]
  (Math/pow (* 4 x (- 1 x))
            k))

(defn power-curve [x a b]
  (let [k (/ (Math/pow (+ a b)
                       (+ a b))
             (* (Math/pow a a)
                (Math/pow b b)))]
    (* k
       (Math/pow x a)
       (Math/pow (- 1 x) b))))

(defn sinc [x k]
  (let [a (* Math/PI
             (- (* k x)
                1))]
    (/ (Math/sin a)
       a)))

(defn quadratic-falloff [x m]
  (let [x2 (/ 1 (Math/pow (+ x 1) 2))
        m2 (/ 1 (Math/pow (+ m 1) 2))]
    (/ (- x2 m2)
       (- 1 m2))))