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
