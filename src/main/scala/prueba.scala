object prueba extends App:
  val estado: Estado = Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, sortearTurno())
  val juego: Jugador = bucleJuego(TableroClasicoLyS, estado)