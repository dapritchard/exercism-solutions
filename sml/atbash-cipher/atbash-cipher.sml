fun encodeChar (c: char): char =
  if Char.isAscii c andalso Char.isLower c then
      chr ((ord #"a") + (ord #"z") - (ord c))
  else if Char.isAscii c andalso Char.isUpper c then
      chr ((ord #"a") + (ord #"Z") - (ord c))
  else
      c

fun decodeChar (c: char): char =
  if Char.isAscii c andalso Char.isLower c then
      chr ((ord #"z") + (ord #"a") - (ord c))
  else if Char.isAscii c andalso Char.isUpper c then
      chr ((ord #"z") + (ord #"A") - (ord c))
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
  let fun encodeImpl [] n sp = []
        | encodeImpl (s::ss) 5 sp = encodeImpl (s::ss) 0 true
        | encodeImpl (s::ss) n sp =
          if s = #" " orelse s = #"," orelse s = #"." then
            encodeImpl ss n sp
          else if sp then
            #" " :: encodeChar s :: encodeImpl ss (n + 1) false
          else
            encodeChar s :: encodeImpl ss (n + 1) false
  in implode (encodeImpl (explode phrase) 0 false)
  end
