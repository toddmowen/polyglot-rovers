(ns sequences.rovers)

;; rover state is represented by a vector, [pos dir]
;; where: pos is the current position, [x y]
;;        dir is the current heading as a unit vector, [x y]

(defn quarter-turn [[x y]] [y (- x)])

;; define compass directions in terms of quarter-turn
(def directions
  (zipmap [:N :E :S :W] (iterate quarter-turn [0 1])))

(def commands
  {:R (fn [[pos dir]] [pos (quarter-turn dir)])
   :L (fn [[pos dir]] [pos (quarter-turn (quarter-turn (quarter-turn dir)))])
   :M (fn [[pos dir]] [(map + pos dir) dir])})

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
