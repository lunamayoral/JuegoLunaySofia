case class Estado(liebre:Posicion, sabuesos:Set[Posicion],turno:Jugador):
  def ocupadas: Set[Posicion] =
    sabuesos ++ Set(liebre)
  
  val nuevoEstado: Estado = Estado(
    liebre = nuevaLiebre,
    sabuesos = estado.sabuesos,
    turno = Jugador.Sabuesos
  )
  
