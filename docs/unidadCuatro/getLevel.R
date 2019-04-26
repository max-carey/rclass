getLevel <- function(nivel) {
  if (nivel == "A1" | nivel == "A2") {
    return("principiante")
  } else if (nivel == "B1" | nivel == "B2") {
    return("intermedio")
  } else {
    return("avanzado")
  }
}