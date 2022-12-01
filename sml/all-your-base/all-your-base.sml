fun createMultiplierImpl (inBase: int) ([]: int list) (hasNonZero: bool): int option =
    if hasNonZero then SOME 1
    else NONE
  | createMultiplierImpl (inBase: int) (x::xs: int list) (hasNonZero: bool): int option =
    if x < 0 then NONE
    else if x = 0 then
      case createMultiplierImpl inBase xs hasNonZero of
          NONE   => NONE
        | SOME m => SOME (inBase * m)
    else if x < inBase then
      case createMultiplierImpl inBase xs true of
          NONE   => NONE
        | SOME m => SOME (inBase * m)
    else NONE

fun createMultiplier (inBase: int) (xs: int list): int option =
    case createMultiplierImpl inBase xs false of
        NONE   => NONE
      | SOME m => SOME (m div inBase)

fun calcLinearCombin (inBase: int) (m: int) ([]: int list): int = 0
  | calcLinearCombin (inBase: int) (m: int) (x::xs: int list): int =
    (m * x) + calcLinearCombin inBase (m div inBase) xs

fun toBase10Int (inBase: int) (digits: int list): int option =
    case createMultiplier inBase digits of
        NONE   => NONE
      | SOME m => SOME (calcLinearCombin inBase m digits)

(* The internal version of `toOutBase` with an additional argument `digits` used
to accumulate the return value *)
fun toOutBaseImpl (outBase: int) (n: int) (acc: int list): int list option =
    if outBase <= 1 then NONE
    else if n < 0 then NONE
    else if n = 0 then SOME acc
    else toOutBaseImpl outBase (n div outBase) (n mod outBase :: acc)

(* Convert from base 10 `n` to the same value but using base `outBase` and where
the return value is represented using an option list of digits *)
fun toOutBase (outBase: int) (n: int): int list option =
    toOutBaseImpl outBase n []

fun rebase (inBase: int, outBase: int, []: int list): int list option     = NONE
  | rebase (inBase: int, outBase: int, digits: int list): int list option =
    case toBase10Int inBase digits of
        NONE   => NONE
      | SOME n => toOutBase outBase n
