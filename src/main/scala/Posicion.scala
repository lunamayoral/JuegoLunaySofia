enum Fila(val Valor: Int) {
  case A extends Fila(1)
  case M extends Fila(0)
  case B extends Fila(-1)
}

enum Columna(val Valor: Int) {
  case I2 extends Columna(-2)
  case I1 extends Columna(-1)
  case M extends Columna(0)
  case D1 extends Columna(1)
  case D2 extends Columna(2)
}

class Posicion(col: Columna, fila: Fila):
  def x: Int = col.Valor
  def y: Int = fila.Valor



