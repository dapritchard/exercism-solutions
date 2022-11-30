(* fun rebase (inBase: int, outBase: int, []: int list): int list option     = NONE *)
(*   | rebase (inBase: int, outBase: int, digits: int list): int list option = *)
(*     if inBase <= 0 then NONE  *)
(*     else if outBase <=0 then NONE *)
(*     else  *)

fun toBase10 (inBase: int) (front: int list) ([]: int list): int option =
    SOME (foldl op+ 0 front)
  | toBase10 (inBase: int) (front: int list) (back::backs: int list): int option =
    if back < 0 then NONE
    else if inBase <= 1 then NONE
    else if inBase <= back then NONE
    else toBase10 inBase (back::(map (fn x => inBase * x) front)) backs

fun toOutBase (outBase: int) (n: int) (digits: int list): int list option =
    if outBase <= 1 then NONE
    else if n < 0 then NONE
    else if n = 0 then SOME digits
    else toOutBase outBase (n div outBase) (n mod outBase::digits)



(* fun rebaseImpl (inBase: int, outBase: int, []: int list, newDigits: int list): int list = newDigits *)
(*   | rebaseImpl (inBase: int, outBase: int, digits: int list, newDigits: int list): int list =                                                                                                *)
(*     if inBase <= 0 then [] *)
(*     else if outBase <=0 then [] *)
(*     else let val lowDigit  = digits mod inBase *)
(*              val newDigits =  *)
(*     in *)
(*     end *)
