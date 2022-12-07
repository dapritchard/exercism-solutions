(* Track whether we've seen:
   - any non-space character
   - any lowercase character
   - any uppercase character
   - the last character is a question mark *)
fun analyzeStatement (hasNonSpace: bool) (hasLow: bool) (hasUpp: bool) (lastQuest: bool) ([]: char list): bool * bool * bool * bool =
    (hasNonSpace, hasLow, hasUpp, lastQuest)
  | analyzeStatement (hasNonSpace: bool) (hasLow: bool) (hasUpp: bool) (lastQuest: bool) (c::cs: char list) =
    if Char.isSpace c then analyzeStatement hasNonSpace hasLow hasUpp lastQuest cs
    else if Char.isLower c then analyzeStatement true true hasUpp false cs
    else if Char.isUpper c then analyzeStatement true hasLow true false cs
    else if c = #"?" then analyzeStatement true hasLow hasUpp true cs
    else analyzeStatement true hasLow hasUpp lastQuest cs

fun response (s: string): string =
    let val (hasNonSpace, hasLow, hasUpp, lastQuest) =
            analyzeStatement false false false false (explode s)
        val isYelled = hasUpp andalso not hasLow
    in
      if not hasNonSpace then "Fine. Be that way!"
      else if not lastQuest andalso not isYelled then "Whatever."
      else if not lastQuest andalso isYelled then "Whoa, chill out!"
      else if lastQuest andalso not isYelled then "Sure."
      else "Calm down, I know what I'm doing!"
    end
