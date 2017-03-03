;; Copyright 2017 Google Inc.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns clfeat.core)

(require '[clojure.test.check :as tc])
(require '[clojure.test.check.generators :as gen])
(require '[clojure.test.check.properties :as prop])

(use '[clfeat.display :only [display-bars]])

(defn counting-sort [xs]
  (let [len (+ (apply max xs) 1)
        counts (int-array len)]
    (doseq [i xs]
      (aset-int counts i (+ 1 (aget counts i))))
    (let [res (transient [])]
      (dotimes [i len]
        (if (pos? (aget counts i))
          (conj! res i)))
      (persistent! res))))

(counting-sort [42])
(counting-sort [2 5 1])

(def counting-sort-correctness
  (prop/for-all [v (gen/vector gen/int)]
                (= (counting-sort v) (sort v))))

(tc/quick-check 100 counting-sort-correctness)

(defn sort-correctness [my-sort]
   (prop/for-all [v (gen/vector gen/int)]
                 (= (my-sort v) (sort v))))

 (defn check-sort [my-sort]
   (tc/quick-check 100 (sort-correctness my-sort)))

(check-sort counting-sort)

(defn counting-sort-2 [xs]
  (if (empty? xs)
    xs
    (let [len (+ (apply max xs) 1)
          counts (int-array len)]
      (doseq [i xs]
        (aset-int counts i (+ 1 (aget counts i))))
      (let [res (transient [])]
        (dotimes [i len]
          (if (pos? (aget counts i))
            (conj! res i)))
        (persistent! res)))))

(check-sort counting-sort-2)

(defn counting-sort-3 [xs]
  (if (empty? xs)
    xs
    (let [maximum (apply max xs)
          minimum (apply min xs)
          len (+ (- maximum minimum) 1)
          counts (int-array len)]
      (doseq [i xs]
        (let [index (- i minimum)]
          (aset-int counts index (+ 1 (aget counts index)))))
      (let [res (transient [])]
        (dotimes [i len]
          (if (pos? (aget counts i))
            (conj! res (+ i minimum))))
        (persistent! res)))))

(check-sort counting-sort-3)

(defn counting-sort-4 [xs]
  (if (empty? xs)
    xs
    (let [maximum (apply max xs)
          minimum (apply min xs)
          len (+ (- maximum minimum) 1)
          counts (int-array len)]
      (doseq [i xs]
        (let [index (- i minimum)]
          (aset-int counts index (+ 1 (aget counts index)))))
      (let [res (transient [])]
        (dotimes [i len]
          (dotimes [_ (aget counts i)]
            (conj! res (+ i minimum))))
        (persistent! res)))))

(check-sort counting-sort-4)

(defrecord Node [left right])

(def example-tree
  (->Node
   (->Node
    nil
    (->Node nil nil))
   (->Node
    (->Node nil nil)
    (->Node nil nil))))

(defn random-tree []
  (if (zero? (rand-int 2))
    nil
    (->Node (random-tree) (random-tree))))

;(display-bars random-tree)

(defn random-tree-2 []
  (if (zero? (rand-int 5))
    nil
    (->Node (random-tree-2) (random-tree-2))))

;(display-bars random-tree-2)

(defn random-tree-3 [depth]
  (cond
    (zero? depth) nil
    (zero? (rand-int 5)) nil
    :else (->Node
           (random-tree-3 (- depth 1))
           (random-tree-3 (- depth 1)))))

;(display-bars #(random-tree-3 5))

(defn random-tree-4 [n]
  (cond
    (zero? n) nil
    (zero? (rand-int 5)) nil
    :else (->Node
           (random-tree-4 (quot n 2))
           (random-tree-4 (quot n 2)))))

;(display-bars #(random-tree-4 30))

(defn random-tree-5 [n]
  (if (zero? n)
    nil
    (let [p (rand-int n)]
      (->Node (random-tree-5 p) (random-tree-5 (- n 1 p))))))

;(display-bars #(random-tree-5 30))

(defrecord LNode [label left right])

(defn rand-triplet-of-sum [n]
  (let [a (rand-int n)
        b (rand-int n)
        c (- n a b)]
    (if (pos? c)
      [a b c]
      [(- n a) (- n b) (- c)])))

(defn random-labelled-tree [n]
  (if (zero? n)
    nil
    (let [[k l r] (rand-triplet-of-sum (- n 1))]
      (->LNode
       k
       (random-labelled-tree l)
       (random-labelled-tree r)))))

; (display-bars #(random-labelled-tree 30))

(defn collect-sizes [tree depth]
  (if (nil? tree)
    nil
    (let [head [depth (:label tree)]
          sleft (collect-sizes (:left tree) (+ depth 1))
          sright (collect-sizes (:right tree) (+ depth 1))]
      (cons head (concat sleft sright)))))

(defn many-sizes [n-trees size]
  (seq
    (frequencies
      (mapcat
        #(collect-sizes % 0)
        (repeatedly n-trees #(random-labelled-tree size))))))

(defn render-sizes [sizes]
  (defn render-size [[[depth size] count]]
    (str \( depth \, size \, count \)))
  (str \[ (clojure.string/join \, (map render-size sizes)) \]))

(spit "/tmp/sizes_bad"
      (render-sizes (many-sizes 10000 100)))

(defn trees-up-to-depth [depth]
  (lazy-seq
   (if (zero? depth)
     (list nil)
     (let [rec (trees-up-to-depth (- depth 1))
           trees (for [l rec r rec] (->Node l r))]
       (cons nil trees)))))

(nth (trees-up-to-depth 40000) 10)

(defn product [s1 s2]
  (for [x s1 y s2]
    (list x y)))

(defn node [[l r]]
  (->Node l r))

(def trees-0 [nil])

(def trees-1
  (map node
       (product trees-0 trees-0)))

(def trees-2
  (map node
       (concat (product trees-0 trees-1)
               (product trees-1 trees-0))))

(def trees-3
  (map node
       (concat (product trees-0 trees-2)
               (product trees-1 trees-1)
               (product trees-2 trees-0))))

(defrecord Finite [card nth])

(defn f-nth [finite index]
  ((:nth finite) index))

(def f-empty
  (->Finite
   0N
   (fn [_] (throw (IndexOutOfBoundsException.)))))

(defn f-singleton [x]
  (->Finite
   1N
   (fn [i]
     (if (zero? i) x (throw (IndexOutOfBoundsException.))))))

(defn f-sum
  ([] f-empty)
  ([f1 & fs]
   (let [f2 (apply f-sum fs)]
     (->Finite
      (+ (:card f1) (:card f2))
      (fn [i]
        (if (< i (:card f1))
          (f-nth f1 i)
          (f-nth f2 (- i (:card f1)))))))))

(defn f-product
  ([f]
   (->Finite
    (:card f)
    (fn [i] (list (f-nth f i)))))
  ([f1 & fs]
   (let [f2 (apply f-product fs)]
     (->Finite
      (* (:card f1) (:card f2))
      (fn [i]
        (cons
         (f-nth f1 (quot i (:card f2)))
         (f-nth f2 (mod i (:card f2)))))))))

(defn f-map [f & finites]
  (let [finite (apply f-product finites)]
    (->Finite
     (:card finite)
     (fn [i]
       (apply f (f-nth finite i))))))

(defn f-gen [f]
  (map #(f-nth f %) (range (:card f))))

(defn trees [n]
  (let [table (make-array Finite (+ n 1))]
    (aset table 0 (f-singleton nil))
    (defn node [i j]
      (f-map ->Node (aget table i) (aget table j)))
    (dotimes [i n]
      (let [is (range 0 (+ i 1))
            js (range i -1 -1)]
        (aset table (+ i 1) (apply f-sum (map node is js)))))
    (aget table n)))

(f-gen (trees 4))
(f-nth (trees 200) 100)

