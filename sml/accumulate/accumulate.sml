fun accumulate (f: ('a -> 'b), []: 'a list): 'b list = []
  | accumulate (f: ('a -> 'b), x :: xs: 'a list): 'b list = f x :: accumulate (f, xs)
