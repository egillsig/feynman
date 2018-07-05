(ns feynman.state)

(defn create-state
  [& {:keys [types dims]
      :or {types 0 dims 0}}] (atom [types dims]))

(defn new-type-variable! [s]
  [:type-variable (-> s
                      (swap! #(vector (+ (first %) 1) (second %)))
                      first)])

(defn new-dimension-variable! [s]
  [:dim-variable (-> s
                     (swap! #(vector (first %)  (+ 1 (second %))))
                     second)])
