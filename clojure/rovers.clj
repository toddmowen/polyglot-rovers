(defn N [vec] (update-in vec [1] (partial + 1)))
(defn E [vec] (update-in vec [0] (partial + 1)))
(defn S [vec] (update-in vec [1] (partial + -1)))
(defn W [vec] (update-in vec [0] (partial + -1)))

(defn turn [quarters bearing]
  (nth
    (drop-while (partial not= bearing) (cycle (list N E S W)))
    (mod quarters 4)))    

(defrecord Rover [position bearing])

(defn M [rover] (update-in rover [:position] (:bearing rover)))
(defn R [rover] (update-in rover [:bearing] (partial turn 1)))
(defn L [rover] (update-in rover [:bearing] (partial turn -1)))

(defn command [rover & cmds] (reduce #(%2 %1) rover cmds))

;; sample data
(command
  (Rover. [1 2] N)
  L M L M L M L M M)
(command
  (Rover. [3 3] E)
  M M R M M R M R R M)
