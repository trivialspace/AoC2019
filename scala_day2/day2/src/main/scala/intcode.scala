case class State(
    position: Int,
    vec: Vector[Long],
    input: List[Int],
    base: Int
) {
  val opcode: Int = vec(position).toInt % 100
  val params = (vec(position) / 100).toInt.toString
    .map(_.asDigit)
    .reverse
    .toList
    .padTo(3, 0)

  def parse(mode: Int, pos: Int): Int = {

    mode match {
      case 1 => pos
      case 0 => vec(pos).toInt
      case 2 => this.vec(pos + this.base).toInt
    }
  }
  val param1 = vec(parse(params(0), position + 1)).toInt
  val param2 = vec(parse(params(1), position + 2)).toInt
  val param3 = parse(params(2), position + 3).toInt

}

object intcode {
  val small = "01101,100,-1,4,0"
  val smallvec = small
    .split(",")
    .map(_.trim())
    .toList
    .map(_.toLong)
    .toVector ++ Vector.fill(10000)(
    0.toLong
  )
  val state1 = State(0, smallvec, List(0), 0)
  def update(state1: State): State = {
    state1.opcode match {
      case 99 => state1
      case 1 =>
        update(
          State(
            state1.position + 4,
            state1.vec.updated(state1.param3, state1.param1 + state1.param2),
            state1.input,
            state1.base
          )
        )
      case 2 =>
        update(
          State(
            state1.position + 4,
            state1.vec.updated(state1.param3, state1.param1 * state1.param2),
            state1.input,
            state1.base
          )
        )
      case 3 => {
        val h :: t = state1.input
        update(
          State(
            state1.position + 2,
            state1.vec.updated(state1.param1, h),
            t,
            state1.base
          )
        )
      }
      case 4 => State(0, Vector(state1.param1), state1.input, state1.base)

    }

  }
  update(state1)
}

object State {
  val temp = List(1, 1)
  temp.padTo(3, 0)
}
