import Jugador.Liebre

import scala.annotation.tailrec

sealed trait MovimientoFicha:
  def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion]

@tailrec
def distancia(acum: Int, pos_sabuesos: List[Posicion], estado: Estado): Int = pos_sabuesos match
  case Nil => acum
  case h :: t => distancia(acum + estado.liebre.manhattan(h), t, estado)

case object MovimientoLiebre extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    movimientosPosiblesLiebre(tablero, estado).map(_._2)

  def movimientosPosiblesLiebre(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)] =
    (tablero.movimientosDesde(estado.liebre) -- estado.sabuesos).map(destino => (estado.liebre, destino))

  def evaluarMovimientoLiebre(tablero: TableroJuego, estado: Estado, destino: Posicion): (Int, Int) =
    val suma_distancia = distancia(0, estado.sabuesos.toList, estado)

    @tailrec
    def sabuesos_sobrepasados(acum: Int, pos_sabuesos: List[Posicion], posicion: Posicion): Int = pos_sabuesos match
      case Nil => acum
      case h :: t => if posicion.x > h.x then sabuesos_sobrepasados(acum+1, t,posicion) else sabuesos_sobrepasados(acum,t,posicion)
    val n_sabuesos_rebasados = sabuesos_sobrepasados(0, estado.sabuesos.toList, estado.liebre)
    
    val primer_valor = 
      if n_sabuesos_rebasados == 0 
        then sabuesos_sobrepasados(0, estado.sabuesos.toList, destino)
        else destino.manhattan(tablero.posicionMetaLiebre)
      
    (primer_valor,suma_distancia)

case object MovimientoSabueso extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    movimientosPosiblesPorSabueso(tablero, estado).map(_._2)  // devolver un set con todas los destinos posibles (solo los destinos)

  def movimientosPosiblesPorSabueso(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)] =
    def sabuesosaux(sabuesos: List[Posicion]): Set[(Posicion, Posicion)] = sabuesos match
      case Nil => Set.empty  // si no hay sabuesos, no hay posiciones
      case cabeza :: cola =>
        val posibles = (tablero.movimientosDesde(cabeza) -- estado.ocupadas)  // quitamos las que están ocupadas
        posibles.filter(destino => destino.x >= cabeza.x).map(destino => (cabeza, destino)) ++ sabuesosaux(cola)

    sabuesosaux(estado.sabuesos.toList)

  // Heurística de los sabuesos
  def evaluarMovimientoSabuesos(tablero: TableroJuego, estado: Estado, origen: Posicion, destino: Posicion): (Int, Int) =
    val distancia_liebre = -destino.manhattan(estado.liebre) // cuanto más cerca mejor, negativo para que la menor distancia sea la mejor

    // Calcular cuántos movimientos tendrá la liebre
    val estadoTemp = Estado(liebre = estado.liebre, sabuesos = estado.sabuesos - origen + destino,
      turno = Jugador.Liebre)

    val movsLiebre = MovimientoLiebre.movimientosPosibles(tablero, estadoTemp).size
    val negmovsLiebre = - movsLiebre // cuantos menos movimientos mejor

    (distancia_liebre, negmovsLiebre)





