sealed trait MovimientoFicha:
  def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    movimientosPosiblesLiebre(tablero, estado).map(_._2)

  def movimientosPosiblesLiebre(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)] =
    (tablero.movimientosDesde(estado.liebre) -- estado.sabuesos).map(destino => (estado.liebre, destino))

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
  // con esto devolvemos las tuplas con (donde esta el sabueso inicial, posible movimiento), habrá varias tuplas con todos los posibles movs