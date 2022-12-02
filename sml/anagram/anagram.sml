(* Copied from
https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Standard_ML. Also see
https://stackoverflow.com/questions/14411862/standard-sorting-functions-in-sml
for a good discussion of various Standard ML sorting implementations *)
fun quicksort ([]: char list): char list = []
  | quicksort (x::xs: char list): char list =
    let val (left, right) = List.partition (fn y => y<x) xs
    in quicksort left @ [x] @ quicksort right
    end

fun quicksortString (s: string): string =
    implode (quicksort (explode s))

fun anagramsFor (subject: string) (candidates: string list): string list =
    let val subjectSorted    = quicksortString subject
        val candidatesSorted = map quicksortString candidates
        fun checkEq s = subjectSorted = s
    in  List.filter checkEq candidatesSorted
    end
