(ns rovers
  (:gen-class))

(require 'clojure.string)

;; equivalent to defn, but add name as metadata to the fn
(defmacro defn-named [sym & decls]
  `(def ~sym ^{:name (str '~sym)} (fn ~@decls)))

;; print-method that uses metadata from defn-named
(let
  [old-print-method (get-method print-method clojure.lang.Fn)]
  (defmethod print-method clojure.lang.Fn [o, ^java.io.Writer w]
    (let
      [name (:name (meta o))]
      (if name
        (.write w name)
        (old-print-method o w)))))

(defn-named N [rover] (update-in rover [:y] inc))
(defn-named E [rover] (update-in rover [:x] inc))
(defn-named S [rover] (update-in rover [:y] dec))
(defn-named W [rover] (update-in rover [:x] dec))

(defn turn [quarters bearing]
  (nth
    (drop-while (partial not= bearing) (list N E S W N E S))
    (mod quarters 4)))    

(defrecord Rover [x y bearing])
(defmethod print-method Rover [o, ^java.io.Writer w]
  (.write w (clojure.string/join " " (map print-str (vals o)))))

(defn-named M [rover] ((:bearing rover) rover))
(defn-named R [rover] (update-in rover [:bearing] (partial turn 1)))
(defn-named L [rover] (update-in rover [:bearing] (partial turn -1)))

(defn command [rover & cmds] (reduce #(%2 %1) rover cmds))

;; sample data
(assert (=
  (Rover. 1 3 N)
  (command
    (Rover. 1 2 N)
    L M L M L M L M M)))
(assert (=
  (Rover. 5 1 E)
  (command
    (Rover. 3 3 E)
    M M R M M R M R R M)))

(defn -main []
  (let [parse (fn [line]
          (map read-string (map first (re-seq #"([0-9]+|.) *" line))))]
    (doseq [line (repeatedly read-line) :while line]
      ;; strictly speaking this is a security hole ("eval is evil")
      (binding [*ns* (find-ns 'rovers)]
        (let [rover (eval (cons 'rovers.Rover. (parse line)))
              cmds  (eval (cons 'list (parse (read-line))))]
          (do (println (apply command rover cmds))))))))
