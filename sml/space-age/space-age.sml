datatype planet = Mercury | Venus | Earth | Mars
                | Jupiter | Saturn | Uranus | Neptune

val secsEarthYear = 31557600.0

fun age_on (Mercury: planet) (seconds: int) : real = Real.fromInt seconds / secsEarthYear / 0.2408467
  | age_on (Venus: planet) (seconds: int) : real = Real.fromInt seconds / secsEarthYear / 0.61519726
  | age_on (Earth: planet) (seconds: int) : real = Real.fromInt seconds / secsEarthYear
  | age_on (Mars: planet) (seconds: int) : real = Real.fromInt seconds / secsEarthYear / 1.8808158
  | age_on (Jupiter: planet) (seconds: int) : real = Real.fromInt seconds / secsEarthYear / 11.862615
  | age_on (Saturn: planet) (seconds: int) : real = Real.fromInt seconds / secsEarthYear / 29.447498
  | age_on (Uranus: planet) (seconds: int) : real = Real.fromInt seconds / secsEarthYear / 84.016846
  | age_on (Neptune: planet) (seconds: int) : real = Real.fromInt seconds / secsEarthYear / 164.79132
