fun isPangram s =
  raise Fail "'isPangram' is not implemented"

val letters =
    [#"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h", #"i", #"j", #"k", #"l", #"m",
     #"n", #"o", #"p", #"q", #"r", #"s", #"t", #"u", #"v", #"w", #"x", #"y", #"z"]

fun remove_letter (c: char) (letters: char list): char list =
    if null letters then []
    else if c = hd letters then tl letters
    else hd letters :: remove_letter c (tl letters)
