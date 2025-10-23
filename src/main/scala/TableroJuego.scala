
trait TableroJuego:

  // --- Devuelve las posiciones accesibles desde una posición ---
  def movimientosDesde(p: Posicion): Set[Posicion]

  // ---  Posiciones iniciales de la liebre y sabuesos ---
  def posicionInicialLiebre: Posicion

  def posicionesInicialesSabuesos: Set[Posicion]

  def posicionMetaLiebre: Posicion

  // --- Pinta el tablero para un estado dado
  def pintarTablero(estado: Estado): Unit

  // --- Comprueba si ha terminado la partida ---
  def esFinPartida(estado: Estado): Option[Jugador]

object TableroClasicoLyS extends TableroJuego:
  // Nodos del tablero
  val I1A: Posicion = Posicion(Columna.I1, Fila.A)
  val MA: Posicion = Posicion(Columna.M, Fila.A)
  val D1A: Posicion = Posicion(Columna.D1, Fila.A)

  val I2M: Posicion = Posicion(Columna.I2, Fila.M)
  val I1M: Posicion = Posicion(Columna.I1, Fila.M)
  val MM: Posicion = Posicion(Columna.M, Fila.M)
  val D1M: Posicion = Posicion(Columna.D1, Fila.M)
  val D2M: Posicion = Posicion(Columna.D2, Fila.M)

  val I1B: Posicion = Posicion(Columna.I1, Fila.B)
  val MB: Posicion = Posicion(Columna.M, Fila.B)
  val D1B: Posicion = Posicion(Columna.D1, Fila.B)

  // Grafo del tablero: lista de adyacencias
  val adyacencias: Map[Posicion, Set[Posicion]] = Map(
    I1A -> Set(MA, I1M, I2M),
    MA -> Set(MM, I1A, D1A),
    D1A -> Set(MA, D1M, D2M, MM),
    I2M -> Set(I1A, I1M, I1B),
    I1M -> Set(I1A, I2M, I1B, MM),
    MM -> Set(I1A, MA, D1A, I1M, D1M, I1B, MB, D1B),
    D1M -> Set(D1A, MM, D1B, D2M),
    D2M -> Set(D1A, D1M, D1B),
    I1B -> Set(I2M, I1M, MM, MB),
    MB -> Set(I1B, MM, D1B),
    D1B -> Set(MB, MM, D1M, D2M)
)

  // --- Devuelve las posiciones accesibles desde una posición ---
  override def movimientosDesde(p: Posicion): Set[Posicion] = adyacencias.getOrElse(p, Set())
  
  // ---  Posiciones iniciales de la liebre y sabuesos ---
  override def posicionInicialLiebre: Posicion = D2M
  override def posicionesInicialesSabuesos: Set[Posicion] = Set(I1A, I2M, I1B)
  override def posicionMetaLiebre: Posicion = I2M

  // --- Pintado ---
  private def pintarNodo(p: Posicion, estado: Estado): String =
    val RESET = "\u001B[0m"
    val ROJO = "\u001B[31m"
    val AZUL = "\u001B[34m"
    val BLANCO = "\u001B[37m"

    if (estado.liebre == p) s"${ROJO}L$RESET"
    else if (estado.sabuesos.contains(p)) s"${AZUL}S$RESET"
    else s"${BLANCO}o$RESET"

  override def pintarTablero(estado: Estado): Unit =
    val s = pintarNodo(_, estado)
    println(s" ${s(I1A)}-----${s(MA)}-----${s(D1A)}")
    println(" ╱ | \\ | / | \\")
    println(s" ${s(I2M)}---${s(I1M)}-----${s(MM)}-----${s(D1M)}---${ s(D2M) }")
    println(" \\ | / | \\ | /")
    println(s" ${s(I1B)}-----${s(MB)}-----${s(D1B)}")

