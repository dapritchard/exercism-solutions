fun rows (input: string): string list =
  let val cs = explode input
  in
    if length cs = 0 orelse length cs >= 2 then []
    else if not (Char.isAscii (hd cs) andalso Char.isUpper (hd cs)) then []
    else
      let val betweenSpace = mkBetweenSpace (hd cs)
          val bottomHalfRows = mkBottomHalfRows (hd cs) [] betweenSpace
      in
        if  then
          else
      end
  end

fun mkBetweenSpace (c: char): char list =
  if not (Char.isAscii c andalso Char.isUpper c) then []
  else if c = #"A" then []
  else let val width = 2 * (ord c - ord #"A") - 1
           fun go n = if n <= 0
                      then []
                      else #" " :: go (n - 1)
  in go width
  end

fun mkRow (rowLetter: char) (beforeSp: char list) (betweenSp: char list): string option =
  if not (Char.isAscii rowLetter andalso Char.isUpper rowLetter) then NONE
  else if rowLetter = #"A" then SOME (implode (beforeSp @ (rowLetter :: beforeSp)))
  else SOME (implode (beforeSp @ (rowLetter :: betweenSp) @ (rowLetter :: beforeSp)))

fun mkBottomHalfRows (c: char) (beforeSpace: char list) ([]: char list) =
    (case mkRow c beforeSpace [] of
         SOME newRow => SOME [newRow]
       | NONE        => NONE)
  | mkBottomHalfRows (c: char) (beforeSpace: char list) (b0::[]: char list) =
      let val newBeforeSpace = #" " :: beforeSpace
      in
        case mkRow c beforeSpace (b0::[]) of
            SOME newRow =>
              (case mkBottomHalfRows (Char.pred c) newBeforeSpace [] of
                   SOME otherRows => SOME (newRow :: otherRows)
                 | NONE           => NONE)
          | NONE => NONE
      end
  | mkBottomHalfRows (c: char) (beforeSpace: char list) (b0::b1::bs: char list) =
      let val newBeforeSpace = #" " :: beforeSpace
      in
        case mkRow c beforeSpace (b0::b1::bs) of
            SOME newRow =>
              (case mkBottomHalfRows (Char.pred c) newBeforeSpace bs of
                 SOME otherRows => SOME (newRow :: otherRows)
               | NONE           => NONE)
          | NONE => NONE
      end
