fun name (SOME input: string option):string = "One for " ^ input ^ ", one for me."
  | name (NONE: string option):string = "One for you, one for me."
