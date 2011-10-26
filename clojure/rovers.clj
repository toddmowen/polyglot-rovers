(ns rovers)

;; equivalent to defn, but add name as metadata to the fn
(defmacro defn-named [name & decls]
  `(def ~name (with-meta (fn ~@decls) {:name (str '~name)})))

;; print-method that uses metadata from defn-named
(let
  [old-print-method (get-method print-method clojure.lang.Fn)]
  (defmethod print-method clojure.lang.Fn [o, ^java.io.Writer w]
    (let
      [name (:name (meta o))]
      (if name
        (.write w name)
        (old-print-method o w)))))

(defn-named N [vec] (update-in vec [1] inc))
(defn-named E [vec] (update-in vec [0] inc))
(defn-named S [vec] (update-in vec [1] dec))
(defn-named W [vec] (update-in vec [0] dec))

(defn turn [quarters bearing]
  (nth
    (drop-while (partial not= bearing) (list N E S W N E S))
    (mod quarters 4)))    

(defrecord Rover [position bearing])

(defn-named M [rover] (update-in rover [:position] (:bearing rover)))
(defn-named R [rover] (update-in rover [:bearing] (partial turn 1)))
(defn-named L [rover] (update-in rover [:bearing] (partial turn -1)))

(defn command [rover & cmds] (reduce #(%2 %1) rover cmds))

;; sample data
(assert (=
  (Rover. [1 3] N)
  (command
    (Rover. [1 2] N)
    L M L M L M L M M)))
(assert (=
  (Rover. [5 1] E)
  (command
    (Rover. [3 3] E)
    M M R M M R M R R M)))
