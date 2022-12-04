(* Copied from
https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Standard_ML. Also see
https://stackoverflow.com/questions/14411862/standard-sorting-functions-in-sml
for a good discussion of various Standard ML sorting implementations *)
fun quicksort ([]: char list): char list = []
  | quicksort (x::xs: char list): char list =
    let val (left, right) = List.partition (fn y => y<x) xs
    in quicksort left @ [x] @ quicksort right
    end

(* Create a sorted version of `s` after converting any uppercase letters to
lowercase *)
fun toSortedLowercase (s: string) =
    implode (quicksort (map Char.toLower (explode s)))

(* Create a version of `s` where any uppercase letters have been converted to
lowercase *)
fun toLower (s: string) =
    implode (map Char.toLower (explode s))

(* Create a tuple with a sorted + lowercase version of `s`, a lowercase version
of `s`, and `s` itself *)
fun makeNormVersions (s: string): string * string * string =
    (toSortedLowercase s, toLower s, s)

fun anagramsFor (subject: string) (candidates: string list): string list =
    let val subjectSorted    = toSortedLowercase subject
        val subjectLower     = toLower subject
        val candidatesSorted = map makeNormVersions candidates
        fun checkEq normed lower =
            subjectSorted = normed andalso not (subjectLower = lower)
        fun filterPairs [] = []
          | filterPairs ((normed, lower, orig) :: ps) =
            if checkEq normed lower then orig :: filterPairs ps
            else filterPairs ps
    in  filterPairs candidatesSorted
    end
