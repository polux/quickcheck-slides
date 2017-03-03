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

(ns clfeat.display)

(require '[incanter.charts :as icharts])
(require '[incanter.core :as icore])

(defn tree-size [tree]
  (cond (nil? tree) 0
        :else (+ 1
                 (tree-size (:left tree))
                 (tree-size (:right tree)))))

(defn wrap [f]
  #(try (f)
        (catch StackOverflowError _ 'infinity)))

(defn tree-frequencies [generator]
  (frequencies (repeatedly 5000 (wrap #(tree-size (generator))))))

(defn plot-frequencies [freqs]
  (let [max-size (min 50 (apply max (filter number? (keys freqs))))
        xs (conj (vec (range (+ max-size 1))) 'infinity)
        ys (map #(get freqs % 0) xs)]
    (icharts/bar-chart xs ys :x-label "size" :y-label "# trees" :legend false)))

(defn display-bars [generator]
  (icore/view (plot-frequencies (tree-frequencies generator))))

; ----

(defn sizes [generator]
  (repeatedly 10000 (wrap #(tree-size (generator)))))

(defn clamp [x y] (if (> y x) x y))

(defn plot-sizes [sizes nbins]
  (icharts/histogram sizes :nbins nbins))

(defn display-histogram [generator nbins]
  (icore/view (plot-sizes (filter number? (sizes generator)) nbins)))

