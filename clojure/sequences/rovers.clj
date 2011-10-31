(ns sequences.rovers)

(defn read-tokens []
  (if-let [line (read-line)]
    (map second (re-seq #"\s*([0-9]+|\w)" line))))

(defn read-plateau []
  (if-let [[xstr ystr] (read-tokens)]
    [(Integer/parseInt xstr) (Integer/parseInt ystr)]))

(defn read-rover-state []
  (if-let [[xstr ystr bstr] (read-tokens)]
     [(Integer/parseInt xstr) (Integer/parseInt ystr) bstr]))

(defn read-rover-commands []
  (read-tokens))

(defn read-rover-state-and-commands []
  (if-let [state (read-rover-state)]
    [state (read-rover-commands)]))

(defn read-plateau-and-rovers []
  [(read-plateau)
   (take-while (complement nil?) (repeatedly read-rover-state-and-commands))])
