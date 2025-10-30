sealed trait MovimientoFicha:
  def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    tablero.movimientosDesde(estado.liebre) -- estado.sabuesos


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
    

