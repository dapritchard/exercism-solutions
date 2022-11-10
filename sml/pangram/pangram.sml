val letters =
  [#"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h", #"i", #"j", #"k", #"l", #"m",
   #"n", #"o", #"p", #"q", #"r", #"s", #"t", #"u", #"v", #"w", #"x", #"y", #"z"]

fun removeLetter (c: char, letters: char list): char list =
  if null letters then []
  else if Char.toLower c = hd letters then tl letters
  else hd letters :: removeLetter (c, tl letters)

fun stripLetters (take: char list): char list =
  foldr removeLetter letters take

fun isPangram s =
    let val letters = explode s
        val remaining = stripLetters letters
    in
        if null remaining then true
        else false
    end
