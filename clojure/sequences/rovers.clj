(ns sequences.rovers)

(defn tokenize [line]
  (map second (re-seq #"\s*([0-9]+|\w)" line)))

(defn parse-plateau [line]
  (let [[xstr ystr] (tokenize line)]
    [(Integer/parseInt xstr) (Integer/parseInt ystr)]))

(defn parse-rover-state [line]
  (let [[xstr ystr bstr] (tokenize line)]
     [(Integer/parseInt xstr) (Integer/parseInt ystr) bstr]))

(defn parse-rover-commands [line]
  (tokenize line))

(defn parse-rovers [lines]
  (for [[state commands] (partition 2 lines)]
    [(parse-rover-state state) (parse-rover-commands commands)]))

(defn parse-plateau-and-rovers [lines]
  [(parse-plateau (first lines))
   (parse-rovers (rest lines))])

(defn read-plateau-and-rovers []
  (let [lines (take-while (complement nil?) (repeatedly read-line))]
    (parse-plateau-and-rovers lines)))
