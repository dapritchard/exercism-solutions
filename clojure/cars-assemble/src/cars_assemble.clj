(ns cars-assemble)

(def success-table
  {1  1.
   2  1.
   3  1.
   4  1.
   5  0.9
   6  0.9
   7  0.9
   8  0.9
   9  0.8
   10 0.77})

(def cars-per-hour 221)

(defn production-rate
  "Returns the assembly line's production rate per hour,
   taking into account its success rate"
  [speed]
  (* speed cars-per-hour (get success-table speed 0.)))

(defn working-items
  "Calculates how many working cars are produced per minute"
  [speed]
  (int (/ (production-rate speed) 60)))
