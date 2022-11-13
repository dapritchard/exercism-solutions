fun collatzImpl (n: int, c: int): int * int =
  if n <= 1 then (n, c)
  else if n mod 2 = 0 then collatzImpl (n div 2, c + 1)
  else collatzImpl (n * 3 + 1, c + 1)

fun collatz (n: int): int option =
  if n <= 0 then NONE
  else SOME (#2 (collatzImpl (n, 0)))
