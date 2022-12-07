fun rows (input: string): string list =
  raise Fail "'rows' is not implemented"

fun mkBetweenSpace (c: char) =
  if not (Char.isAscii c andalso Char.isUpper c) then []
  else if c = #"A" then []
  else let val width = 2 * (ord c - ord #"A") - 1
           fun go n = if n <= 0 then []
                      else #" " :: go (n - 1)
  in go width
  end

fun mkRow (rowLetter: char) (beforeSp: char list) (betweenSp: char list): string  =
  if not (Char.isAscii c andalso Char.isUpper c) then ""
  else if rowLetter = #"A" then implode (beforeSp @ (rowLetter :: beforeSp))
  else implode (beforeSp @ (rowLetter :: betweenSp) @ (rowLetter :: beforeSp))
