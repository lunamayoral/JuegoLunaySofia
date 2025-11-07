import scala.annotation.tailrec

sealed trait MovimientoFicha:
  def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    movimientosPosiblesLiebre(tablero, estado).map(_._2)

  def movimientosPosiblesLiebre(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)] =
    (tablero.movimientosDesde(estado.liebre) -- estado.sabuesos).map(destino => (estado.liebre, destino))

  def evaluarMovimiento(tablero: TableroJuego, estado: Estado, destino: Posicion): (Int, Int) =
    @tailrec
    def distancia(acum:Int, pos_sabuesos:List[Posicion]):Int= pos_sabuesos match
      case Nil => acum
      case h::t => distancia(acum + estado.liebre.manhattan(h), t)
    val suma_distancia = distancia(0, estado.sabuesos.toList)

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
  // con esto devolvemos las tuplas con (donde esta el sabueso inicial, posible movimiento), habrá varias tuplas con todos los posibles movs