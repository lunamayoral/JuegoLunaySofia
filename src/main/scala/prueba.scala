import scala.annotation.tailrec

object prueba extends App:
  println("Hola! Bienvenido al juego de liebre y sabuesos!")
  println("Quieren jugar? (s/n)") //si escribe otra cosa, entiendo como que no quiere jugar
  if scala.io.StdIn.readLine() == "s" then
    @tailrec
    def juego(n_partidas:Int, ganancia_liebre:Int, ganancia_sabuesos:Int):Unit=
      println(s"Partida $n_partidas !")
      println("Cuántos jugadores juegan?")
      println("[0]: Buen gusto con ver la película de la batalla entre IAs!")
      println("[1]: Épica batalla de tú contra IA!")
      println("[2]: Juega con tu amigo, a disfrutar!")
      val n_jugador = comprobacionTeclado(3)
      val estado: Estado = Estado(TableroClasicoLyS.posicionInicialLiebre, TableroClasicoLyS.posicionesInicialesSabuesos, sortearTurno())
      val ganador = if n_jugador == 0 
      then bucleJuego(TableroClasicoLyS,estado, Set(Jugador.Liebre, Jugador.Sabuesos))
      else if n_jugador == 2 then bucleJuego(TableroClasicoLyS,estado, Set.empty)
      else
        println("Qué quieres jugar?")
        println("[0]: Sabuesos")
        println("[1]: Liebre")
        if comprobacionTeclado(2) == 0 
        then bucleJuego(TableroClasicoLyS,estado, Set(Jugador.Liebre))
        else bucleJuego(TableroClasicoLyS,estado, Set(Jugador.Sabuesos))
      println("Se ha acabado el juego!")
      println("Quieren jugar otra partida? (s/n)")
      if scala.io.StdIn.readLine() == "s" 
      then if ganador == Jugador.Liebre 
        then juego(n_partidas + 1, ganancia_liebre + 1, ganancia_sabuesos)
        else juego(n_partidas + 1, ganancia_liebre, ganancia_sabuesos + 1)
      else
        if ganador == Jugador.Liebre
        then 
          println(s"La liebre ha ganado ${ganancia_liebre+1} veces!")
          println(s"Los sabuesos han ganado $ganancia_sabuesos veces!")
        else 
          println(s"Los sabuesos han ganado ${ganancia_sabuesos+1} veces!")
          println(s"La liebre ha ganado $ganancia_liebre veces!")
        println("Buen día!")
    juego(0,0,0)
  else
    println("Adiós! Buen día!")