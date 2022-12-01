(* The internal version of `createMultiplierImpl` with an additional argument
`hasNonZero` that is used to track whether any of the digits that have been seen
are nonzero. When the return value is non-NONE it has the value of
inBase^(length (x::xs)). *)
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

(* Construct the option multiplier of the highest-order digit in `digits` for
base `inBase`. So for example, if the base was 2 and the length of the digits
list was 4, then the return value would be 2^3. Returns NONE if any of the
following are true:
  * Any of the digits are less than 0
  * Any of the digits are greater than or equal to inBase
  * None of the digits are nonzero *)
fun createMultiplier (inBase: int) (digits: int list): int option =
    case createMultiplierImpl inBase digits false of
        NONE   => NONE
      | SOME m => SOME (m div inBase)

(* Compute the linear combination of multipliers and digits. For example,
   calcLinearCombin 2 32 [1, 0, 1, 0, 1, 0]
corresponds to the linear combination
   1*32  + 0*16  + 1*8   + 0*4   + 1*2   + 0*1
or equivalently
   1*2^5 + 0*2^4 + 1*2^3 + 0*2^2 + 1*2^1 + 0*2^0 *)
fun calcLinearCombin (inBase: int) (m: int) ([]: int list): int = 0
  | calcLinearCombin (inBase: int) (m: int) (x::xs: int list): int =
    (m * x) + calcLinearCombin inBase (m div inBase) xs

(* Create a base-10 int from its base `inBase` representation where `digits` is
a list of the digits of the number *)
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

(* Convert from base-10 `n` to the same value but using base `outBase` and where
the return value is represented using an option list of digits *)
fun toOutBase (outBase: int) (n: int): int list option =
    toOutBaseImpl outBase n []

(* Convert a list of digits `digits` representing a number in base `inBase` to
an option list of digits representing the same number in base `outBase` *)
fun rebase (inBase: int, outBase: int, []: int list): int list option     = NONE
  | rebase (inBase: int, outBase: int, digits: int list): int list option =
    case toBase10Int inBase digits of
        NONE   => NONE
      | SOME n => toOutBase outBase n
