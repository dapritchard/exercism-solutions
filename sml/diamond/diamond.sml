(* If the input is a capital letter then create a SOME list with every element a
space such that the length of the list is equal to the number of spaces that
should go betwween the letters on that row. Otherwise return NONE.

For example the #"A" row has 0 spaces between the sole letter, the #"B" row has
1 space between the letters, the #"C" row has 3 spaces between the letters, and
so on. *)
fun mkBetweenSpace (rowLetter: char): char list option =
  if not (Char.isAscii rowLetter andalso Char.isUpper rowLetter) then NONE
  else if rowLetter = #"A" then SOME []
  else let val width = 2 * (ord rowLetter - ord #"A") - 1
           fun go n = if n <= 0 then [] else #" " :: go (n - 1)
  in SOME (go width)
  end

(* If `rowLetter` is a capital letter then create a SOME string such that given
appropriate values for `beforeSpace` and `betweenSpace` the string represents a
row in the diamond. Otherwise return NONE.

- The first input is the character that should be placed in the current row.
- The second input is a list of spaces such that the length of the list is equal
  to the number of spaces *before* the first letter in the row.
- The third input is a list of spaces such that the length of the list is equal
  to the number of spaces *between* the first letter and the second letter in
  the row. *)
fun mkRow (rowLetter: char) (beforeSpace: char list) (betweenSpace: char list): string option =
  if not (Char.isAscii rowLetter andalso Char.isUpper rowLetter) then NONE
  else if rowLetter = #"A" then SOME (implode (beforeSpace @ (rowLetter :: beforeSpace)))
  else SOME (implode (beforeSpace @ (rowLetter :: betweenSpace) @ (rowLetter :: beforeSpace)))

(* If `rowLetter` is a capital letter then create a SOME list representing rows
for the diamond in descending order. Otherwise return NONE. The formal arguments
have the same interpretation as for `mkRow`.

So for example, given an input for `rowLetter` of #"C" and appropriate inputs
for the second and third inputs, then this function constructs a SOME list with
entries representing the #"C" row, "#B" row, and #"A" row, in that order. *)
fun mkDescendingRows (rowLetter: char) (beforeSpace: char list) ([]: char list): string list option =

  (* This case corresponds to the #"A" row *)
  (case mkRow rowLetter beforeSpace [] of
      SOME newRow => SOME [newRow]
    | NONE        => NONE)

  (* This case corresponds to the #"B" row and is a special case because we only
  remove one space between the letters *)
  | mkDescendingRows (rowLetter: char) (beforeSpace: char list) (b0::[]: char list) =
      let val newBeforeSpace = #" " :: beforeSpace
      in
        case mkRow rowLetter beforeSpace (b0::[]) of
            SOME newRow =>
              (case mkDescendingRows (Char.pred rowLetter) newBeforeSpace [] of
                   SOME otherRows => SOME (newRow :: otherRows)
                 | NONE           => NONE)
          | NONE => NONE
      end

  (* This case corresponds to any letter after (but not including) #"rowLetter" *)
  | mkDescendingRows (rowLetter: char) (beforeSpace: char list) (b0::b1::betweenSpace: char list) =
      let val newBeforeSpace = #" " :: beforeSpace
      in
        case mkRow rowLetter beforeSpace (b0::b1::betweenSpace) of
            SOME newRow =>
              (case mkDescendingRows (Char.pred rowLetter) newBeforeSpace betweenSpace of
                 SOME otherRows => SOME (newRow :: otherRows)
               | NONE           => NONE)
          | NONE => NONE
      end

fun rows (input: string): string list =
  let val cs = explode input
  in
    if length cs = 0 orelse length cs >= 2 then []
    else if not (Char.isAscii (hd cs) andalso Char.isUpper (hd cs)) then []
    else
      case mkBetweenSpace (hd cs) of
          NONE => []
        | SOME betweenSpace =>
            (case mkDescendingRows (hd cs) [] betweenSpace of
                 NONE         => []
               | SOME []      => []
               | SOME (r::[]) => [r]
               | SOME (r::rs) => (rev rs) @ (r::rs))
  end
