
case class Estado(pos_liebre:Posicion, pos_sabuesos:Set[Posicion],turno:Jugador):
  def ocupadas: Set[Posicion] =
    pos_sabuesos ++ Set(pos_liebre)

/* se usa así
val nuevoEstado: Estado = Estado(
  liebre = nuevaLiebre,
  sabuesos = estado.sabuesos,
  turno = Jugador.Sabuesos
)*/
