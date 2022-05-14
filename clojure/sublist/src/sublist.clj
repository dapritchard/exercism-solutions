(ns sublist)

(defn check-startswith
  "Check whether the k-th element of `list1` is equal to the k-th element of
  `list2` for each element in `list1`. If the length of `list2` is shorter than
  `list1` then `false` is returned"
  [list1 list2]
  (cond
    (empty? list1) true
    (empty? list2) false
    (= (first list1) (first list2)) (check-startswith
                                     (rest list1)
                                     (rest list2))))

(defn check-sublist
  "Check whether `list1` is a sublist of `list2`"
  [list1 list2]
  (cond
    (> (count list1) (count list2)) false
    (check-startswith list1 list2) true
    :else (check-sublist list1 (rest list2))))

(defn classify
  "Determine which one of the relations `list1` is with regards to `list2`: a
  `:sublist`, a `:superlist`, `:equal`, or `:unequal`"
  [list1 list2]
  (cond
    (= (count list1) (count list2)) (if (= list1 list2)
                                      :equal
                                      :unequal)
    (< (count list1) (count list2)) (if (check-sublist list1 list2)
                                      :sublist
                                      :unequal)
    :else (if (check-sublist list2 list1)
            :superlist
            :unequal)))
