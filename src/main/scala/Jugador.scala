import scala.util.Random

enum Jugador:
  case Liebre, Sabuesos

def sortearTurno():Jugador =
  if Random.nextBoolean() then Jugador.Liebre else Jugador.Sabuesos
