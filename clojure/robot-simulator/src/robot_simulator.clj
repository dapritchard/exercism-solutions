(ns robot-simulator)

(defn robot [coordinates bearing]
  {:coordinates coordinates
   :bearing bearing}
  )

(defn turn-right [bearing]
  (def right-actions
    {:north :east
     :east :south
     :south :west
     :west :north})
  (bearing right-actions))

(defn turn-left [bearing]
  (def left-actions
    {:north :west
     :west :south
     :south :east
     :east :north})
  (bearing left-actions))

(defn advance [robot-val]
  (let [coordinates (:coordinates robot-val)
        x (:x coordinates)
        y (:y coordinates)
        bearing (:bearing robot-val)]
    (cond
      (= bearing :north) (robot {:x x
                                 :y (+ y 1)}
                                bearing)
      (= bearing :east) (robot {:x (+ x 1)
                                :y y}
                               bearing)
      (= bearing :south) (robot {:x x
                                 :y (- y 1)}
                                bearing)
      (= bearing :west) (robot {:x (- x 1)
                                :y y}
                               bearing))))

(defn simulate [directives robot-val]
  (defn turn-right-robot [robot-val]
    (robot (:coordinates robot-val) (turn-right (:bearing robot-val))))
  (defn turn-left-robot [robot-val]
    (robot (:coordinates robot-val) (turn-left (:bearing robot-val))))
  (def action-map
    {"L" turn-left-robot
     "R" turn-right-robot
     "A" advance})
  (let [directives-split (clojure.string/split directives #"")
        directives-fcns (map #(get action-map %) directives-split)
        robot-new (reduce #(%2 %1) robot-val directives-fcns)]
    robot-new))
