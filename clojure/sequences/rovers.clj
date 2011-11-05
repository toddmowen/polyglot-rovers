(ns sequences.rovers
  (:gen-class))

(require 'clojure.set)

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
  (let [[xstr ystr] (tokenize line)
        x (Integer/parseInt xstr)
        y (Integer/parseInt ystr)]
    [x y]))

(defn parse-rover-state [line]
  (let [[xstr ystr dstr] (tokenize line)
        x (Integer/parseInt xstr)
        y (Integer/parseInt ystr)
        dir (or
              (directions (keyword dstr))
              (throw (new Exception "No such direction")))]
    [[x y] dir]))

(defn parse-rover-commands [line]
  (map
    (fn [cstr]
      (or
        (commands (keyword cstr))
        (throw (new Exception "No such command"))))
    (tokenize line)))

(defn parse-rovers [lines]
  (for [[state commands] (partition 2 lines)]
    [(parse-rover-state state) (parse-rover-commands commands)]))

(defn parse-plateau-and-rovers [[plateau-line & rover-lines]]
  [(parse-plateau plateau-line) (parse-rovers rover-lines)])

(defn read-plateau-and-rovers []
  (let [lines (take-while (complement nil?) (repeatedly read-line))]
    (parse-plateau-and-rovers lines)))

(defn rover-end-states [[plateau rovers-and-commands]]
  (for [[rover commands] rovers-and-commands]
    (reduce #(%2 %1) rover commands)))

(defn prn-rover [rover]
  (let [[[x y] dir] rover
        dirword ((clojure.set/map-invert directions) dir)]
    (println x y (name dirword))))

(defn -main []
  (dorun (map prn-rover (rover-end-states (read-plateau-and-rovers)))))
