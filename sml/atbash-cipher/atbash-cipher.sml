fun encodeChar (c: char): char =
  if Char.isAscii c andalso Char.isLower c then
      chr ((ord #"a") + (ord #"z") - (ord c))
  else if Char.isAscii c andalso Char.isUpper c then
      chr ((ord #"A") + (ord #"Z") - (ord c))
  else
      c

fun decodeChar (c: char): char =
  if Char.isAscii c andalso Char.isLower c then
      chr ((ord #"z") + (ord #"a") - (ord c))
  else if Char.isAscii c andalso Char.isUpper c then
      chr ((ord #"Z") + (ord #"A") - (ord c))
  else
      c

fun decode (phrase: string): string =
  let fun decodeImpl [] = []
        | decodeImpl (s::ss) =
          if s = #" " then decodeImpl ss
          else decodeChar s :: decodeImpl ss
  in implode (decodeImpl (explode phrase))
  end

fun encode (phrase: string): string =
  raise Fail "'encode' is not implemented"
