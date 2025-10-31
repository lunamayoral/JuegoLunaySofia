import scala.annotation.tailrec

@tailrec
def pintarMovimientosPosibles(pos: List[(Posicion, Posicion)], i: Int = 0): Unit = pos match
  case Nil => println("")
  case (pos1,pos2) :: t =>
    print(s"[$i] $pos1 -> $pos2  ")
    pintarMovimientosPosibles(t, i + 1)

@tailrec
def bucleJuego(tablero: TableroJuego, estado: Estado): Jugador =
  //1.pintar tablero
  tablero.pintarTablero(estado)
  
  //2. Calcular movimiento posible
  val mov_pos = if estado.turno == Jugador.Liebre then
    MovimientoLiebre.movimientosPosiblesLiebre(tablero, estado)
  else
    MovimientoSabueso.movimientosPosiblesPorSabueso(tablero, estado)
    
  //3.mostrar los movimientos posibles
  println(s"Turno de ${estado.turno}: elige posicion a mover. ")
  pintarMovimientosPosibles(mov_pos.toList)
  
  //4.entrada teclado
  println("Introduce el número del movimiento elegido:")
  val eleccion = scala.io.StdIn.readLine().toInt
  val origen = mov_pos.toList(eleccion)._1
  val destino = mov_pos.toList(eleccion)._2
  
  //5.ejecuta movimiento
  val nuevoEstado: Estado = 
    if estado.turno == Jugador.Liebre then Estado(
      liebre = destino,
      sabuesos = estado.sabuesos,
      turno = Jugador.Sabuesos)
    else Estado(
      liebre = estado.liebre,
      sabuesos = estado.sabuesos + destino - origen,
      turno = Jugador.Liebre)
    
  //6.comprobar fin de partida
  tablero.esFinPartida(nuevoEstado) match
    case Some(ganador) =>
      tablero.pintarTablero(nuevoEstado)
      println(s"\n¡Partida terminada! Ganador: $ganador")
      ganador
    case None =>
      bucleJuego(tablero, nuevoEstado)
