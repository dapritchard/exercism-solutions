fun toBase10 (inBase: int) (processed: int list) ([]: int list): int option =
    SOME (foldl op+ 0 processed)
  | toBase10 (inBase: int) (processed: int list) (back::backs: int list): int option =
    if back < 0 then NONE
    else if inBase <= 1 then NONE
    else if inBase <= back then NONE
    else toBase10 inBase (back :: (map (fn x => inBase * x) processed)) backs

(* The internal version of `toOutBase` with an additional argument `digits` used
to accumulate the return value *)
fun toOutBaseImpl (outBase: int) (n: int) (acc: int list): int list option =
    if outBase <= 1 then NONE
    else if n < 0 then NONE
    else if n = 0 then SOME acc
    else toOutBaseImpl outBase (n div outBase) (n mod outBase :: acc)

(* Convert from base 10 digit `n` to the same value but with base `outBase` and
where the return value is represented using an option list of digits *)
fun toOutBase (outBase: int) (n: int): int list option =
    toOutBaseImpl outBase n []

fun rebase (inBase: int, outBase: int, []: int list): int list option     = NONE
  | rebase (inBase: int, outBase: int, digits: int list): int list option =
    if inBase <= 1 then NONE
    else if outBase <= 1 then NONE
    else
      case toBase10 inBase [] digits of
          NONE   => NONE
        | SOME n => toOutBase outBase n
