import scala.annotation.tailrec

@tailrec
def pintarMovimientosPosibles(pos: List[Posicion], i: Int = 0): Unit = pos match
  case Nil => println("")
  case h :: t =>
    print(s"[$i] $h  ")
    pintarMovimientosPosibles(t, i + 1)

@tailrec
def bucleJuego(tablero: TableroJuego, estado: Estado): Jugador =
  //1.pintar tablero
  tablero.pintarTablero(estado)
  
  //2. Calcular movimiento posible
  val mov_pos = if estado.turno == Jugador.Liebre then
    MovimientoLiebre.movimientosPosibles(tablero, estado)
  else
    MovimientoSabueso.movimientosPosibles(tablero, estado)
    
  //3.mostrar los movimientos posibles
  println(s"Turno de ${estado.turno}: elige posicion a mover. ")
  pintarMovimientosPosibles(mov_pos.toList)
  
  //4.entrada teclado
  println("Introduce el número del movimiento elegido:")
  //cual sabueso a mover
  val eleccion = scala.io.StdIn.readLine().toInt
  val destino = mov_pos.toList(eleccion)
  
  //5.ejecuta movimiento
  val nuevoEstado: Estado = 
    if estado.turno == Jugador.Liebre then Estado(
      liebre = destino,
      sabuesos = estado.sabuesos,
      turno = Jugador.Sabuesos)
    else Estado(
      liebre = estado.liebre,
      sabuesos = estado.sabuesos + destino, //hay que quitar el sabueso elegido
      turno = Jugador.Liebre)
    
  //6.comprobar fin de partida
  tablero.esFinPartida(nuevoEstado) match
    case Some(ganador) =>
      tablero.pintarTablero(nuevoEstado)
      println(s"\n¡Partida terminada! Ganador: $ganador")
      ganador
    case None =>
      bucleJuego(tablero, nuevoEstado)
