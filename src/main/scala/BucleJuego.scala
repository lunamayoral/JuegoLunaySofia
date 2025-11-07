import scala.annotation.tailrec

@tailrec
def pintarMovimientosPosibles(pos: List[(Posicion, Posicion)], i: Int = 0): Unit = pos match
  case (pos1,pos2) :: t =>
    println(s"[$i] $pos1 -> $pos2  ")
    pintarMovimientosPosibles(t, i + 1)
  case Nil =>

@tailrec
def bucleJuego(tablero: TableroJuego, estado: Estado, modoIA: Boolean): Jugador =
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

  val destino = if modoIA && estado.turno == Jugador.Liebre
  then
    def mov_liebre_IA(mov_pos:List[(Posicion,Posicion)], mejor:(Posicion,Posicion)):(Posicion,Posicion) = mov_pos match
      case Nil => mejor
      case h::t =>
        val m1 = MovimientoLiebre.evaluarMovimiento(tablero,estado,h._2)
        val m2 = MovimientoLiebre.evaluarMovimiento(tablero,estado,mejor._2)
        if m1._1 > m2._1
        then mov_liebre_IA(t, h)
        else if m1._1 == m2._1 then
          if m1._2 > m2._2
          then mov_liebre_IA(t, h)
          else mov_liebre_IA(t, mejor)
        else mov_liebre_IA(t, mejor)
    val dest = mov_liebre_IA(mov_pos.toList, mov_pos.toList.head)
    println(s"La IA liebre mueve a ${dest._2}")
    dest
  else
    //4.entrada teclado
    println("Introduce el número del movimiento elegido:")
    val eleccion = scala.io.StdIn.readLine().toInt
    mov_pos.toList(eleccion)

    //5.ejecuta movimiento
  val nuevoEstado: Estado =
    if estado.turno == Jugador.Liebre then Estado(
      liebre = destino._2,
      sabuesos = estado.sabuesos,
      turno = Jugador.Sabuesos)
    else Estado(
      liebre = estado.liebre,
      sabuesos = estado.sabuesos + destino._2 - destino._1,
      turno = Jugador.Liebre)

  //6.comprobar fin de partida
  tablero.esFinPartida(nuevoEstado) match
    case Some(ganador) =>
      tablero.pintarTablero(nuevoEstado)
      println(s"\n¡Partida terminada! Ganador: $ganador")
      ganador
    case None =>
        bucleJuego(tablero, nuevoEstado, modoIA)

/* bucle del juego sin IA (1 version)
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
*/
