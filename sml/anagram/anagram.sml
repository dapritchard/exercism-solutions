(* Copied from
https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Standard_ML. Also see
https://stackoverflow.com/questions/14411862/standard-sorting-functions-in-sml
for a good discussion of various Standard ML sorting implementations *)
fun quicksort ([]: char list): char list = []
  | quicksort (x::xs: char list): char list =
    let val (left, right) = List.partition (fn y => y<x) xs
    in quicksort left @ [x] @ quicksort right
    end

fun quicksortString (s: string) =
    implode (quicksort (explode s))

(* TODO: include tuple in type definition *)
fun makeNormPair (s: string) =
    (implode (quicksort (map Char.toLower (explode s))), s)

(* fun anagramsFor (subject: string) (candidates: string list): string list = *)
fun anagramsFor (subject: string) (candidates: string list) =
    let val subjectSorted    = quicksortString subject
        val candidatesSorted = map makeNormPair candidates
        fun checkEq normed orig =
            subjectSorted = normed andalso not (subject = orig)
        fun filterPairs [] = []
          | filterPairs ((normed, orig) :: ps) =
            if checkEq normed orig then orig :: filterPairs ps
            else filterPairs ps
    in  filterPairs candidatesSorted
    end
