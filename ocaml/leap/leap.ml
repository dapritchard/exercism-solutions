open Base

let leap_year year =
  (year % 4 = 0)
  && ((not (year % 100 = 0))
      || (year % 400 = 0))
