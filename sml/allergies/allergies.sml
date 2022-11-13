datatype allergen = Eggs
                  | Peanuts
                  | Shellfish
                  | Strawberries
                  | Tomatoes
                  | Chocolate
                  | Pollen
                  | Cats

fun allergicTo (score: int) (Eggs: allergen): bool         = score           mod 2 = 1
  | allergicTo (score: int) (Peanuts: allergen): bool      = (score div   2) mod 2 = 1
  | allergicTo (score: int) (Shellfish: allergen): bool    = (score div   4) mod 2 = 1
  | allergicTo (score: int) (Strawberries: allergen): bool = (score div   8) mod 2 = 1
  | allergicTo (score: int) (Tomatoes: allergen): bool     = (score div  16) mod 2 = 1
  | allergicTo (score: int) (Chocolate: allergen): bool    = (score div  32) mod 2 = 1
  | allergicTo (score: int) (Pollen: allergen): bool       = (score div  64) mod 2 = 1
  | allergicTo (score: int) (Cats: allergen): bool         = (score div 128) mod 2 = 1

val allergensOrder: allergen list =
  [ Eggs
  , Peanuts
  , Shellfish
  , Strawberries
  , Tomatoes
  , Chocolate
  , Pollen
  , Cats
  ]

fun allergiesImpl (n: int) (has: allergen list) (potential: allergen list): allergen list =
  if n <= 0 orelse null potential then has
  else if n mod 2 = 1 then allergiesImpl (n div 2) (hd potential :: has) (tl potential)
  else allergiesImpl (n div 2) has (tl potential)

fun allergies (score: int): allergen list =
  rev (allergiesImpl score [] allergensOrder)
