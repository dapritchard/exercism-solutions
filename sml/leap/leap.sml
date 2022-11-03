fun divBy n d = n mod d = 0

fun isLeapYear year =
    if not (divBy year 4) then false   (* common case first for efficiency *)
    else if divBy year 400 then true   (* this case is always a leap year *)
    else if divBy year 100 then false  (* we know prior to this that the year is divisible by 4 but not 100 *)
    else true
