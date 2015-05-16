(ns bonjai.tree
  (:require [fast-zip.core :as z]
            [clojure.set :refer [map-invert]]))

(def match-var? symbol?)

(defn build-map-matchers [value pattern]
  (let [matchers
        (apply
         map vector
         (for [[k v] pattern]
           (if (match-var? k)
             [[v (get (map-invert value) v ::none)] [v k]]
             [[k (get value k ::none)] [k v]])))]
    (when-not (some #(= % ::none) (->> matchers first (map second)))
      matchers)))

(defn match [value pattern]
  (loop [[value-zipper pattern-zipper :as both-zippers] (map #(z/zipper coll? seq {} [%]) [value pattern])
         result (transient {})]
    
    (let [[value-node pattern-node :as both-nodes] (map z/node both-zippers)]
      (condp apply [z/end? both-zippers]
        every? (when (seq both-zippers) (persistent! result)) ;;both zippers exhausted at the same time, result!
        some nil ;;reached the end of one of the zippers when the other one still had stuff left
        (recur
         (cond
           (every? map? both-nodes)
           (map (comp z/next z/replace)
                both-zippers
                (build-map-matchers value-node pattern-node))

           (or (every? z/branch? both-zippers) (= value-node pattern-node))
           (map z/next both-zippers)
           
           (= '& pattern-node)
           [(z/replace (z/up value-zipper)   (cons value-node (z/rights value-zipper)))
            (z/replace (z/up pattern-zipper) (-> pattern-zipper z/right z/node))]
           
           :else
           (when (condp some [pattern-node]
                   fn? (pattern-node value-node)
                   set? (contains? pattern-node value-node)
                   keyword? (or (= pattern-node value-node) (pattern-node value-node) (contains? value-node pattern-node))
                   #(instance? java.util.regex.Pattern %) (re-find pattern-node (str value-node))
                   '#{_} true
                   symbol? (if-let [f (resolve pattern-node)]
                             (f value-node)
                             (when (= value-node (result pattern-node value-node))
                               (assoc! result pattern-node value-node)))
                   nil)
             (map #(z/replace % nil) both-zippers)))
         (or (some-> pattern-node meta :tag (#(assoc! result % value-node))) result))))))

