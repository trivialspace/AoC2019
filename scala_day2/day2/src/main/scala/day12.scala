object day12 {
  class moon(px: Int, py: Int, pz: Int, vx: Int, vy: Int, vz: Int) {
    // def this(position:Boolean, values:List[Int]){
    //     if (position) this(values(0), values(1), values(2), 0,0,0) else this(0,0,0, values(0), values(1), values(2))
    // }
    def this(position: List[Int], velocity: List[Int]) {
      this(
        position(0),
        position(1),
        position(2),
        velocity(0),
        velocity(1),
        velocity(2)
      )
    }
    def position(): List[Int] = { List(px, py, pz) }
    def velocity(): List[Int] = { List(vx, vy, vz) }

    def updateV(update: List[Int]): moon = {
      new moon(
        this.position(),
        (this.velocity.zip(update).map({ case (a, b) => a + b }))
      )
    }
    def updateP(update: List[Int]): moon = {
      new moon(
        this.position().zip(update).map({ case (a, b) => a + b }),
        this.velocity()
      )
    }
    def potential(): Int = { this.position.map(x => math.abs(x)).sum }
    def kinetic(): Int = { this.velocity.map(x => math.abs(x)).sum }
    def energy(): Int = { this.potential() * this.kinetic() }
    def toxTup(): (Int, Int) = { (px, vx) }
    def toyTup(): (Int, Int) = { (py, vy) }
    def tozTup(): (Int, Int) = { (pz, vz) }
  }

  val moon1 = new moon(List(-16, -1, -12), List(0, 0, 0))
  val moon2 = new moon(0, -4, -17, 0, 0, 0)
  val moon3 = new moon(List(-11, 11, 0), List(0, 0, 0))
  val moon4 = new moon(List(2, 2, -6), List(0, 0, 0))

  val allMoons: List[moon] = List(moon1, moon2, moon3, moon4)
  val smallMoons = List(
    new moon(List(-1, 0, 2), List(0, 0, 0)),
    new moon(List(2, -10, -7), List(0, 0, 0)),
    new moon(List(4, -8, 8), List(0, 0, 0)),
    new moon(List(3, 5, -1), List(0, 0, 0))
  )

  def gravityUpdate(m1: moon, m2: moon): List[Int] = {
    val both = m1.position.zip(m2.position)
    for (coord <- both) yield math.signum(coord._2 - coord._1)

  }

  def velocityUpdate(m1: moon): moon = {
    m1.updateP(m1.velocity)
  }

  def update(allMoons: List[moon]): List[moon] = {
    val half = for (m1 <- allMoons) yield {
      m1.updateV(allMoons.map(gravityUpdate(m1, _)).transpose.map(_.sum))
    }
    half.map(velocityUpdate(_))
  }
  val test = update(allMoons)
  scala.collection.Iterator
    .iterate(smallMoons)(update)
    .drop(2772)
    .next
    .map(x => println(x.position))
  val resultList =
    scala.collection.Iterator.iterate(allMoons)(update).drop(1000).next
  resultList.foreach(x => println(x.position))

  resultList.map(x => x.energy()).sum
  val history1 = Set(allMoons.map(x => x.toxTup()))
  val history2 = Set(allMoons.map(x => x.toyTup()))
  val history3 = Set(allMoons.map(x => x.tozTup()))
  val temp = allMoons.map(x => x.toxTup())
  val temp2 = allMoons.map(x => x.toyTup())
  val temp3 = allMoons.map(x => x.tozTup())
  scala.collection.Iterator
    .iterate(update(allMoons))(update)
    .takeWhile(_.map(x => x.toxTup()) != temp)
    .size
  def cont(
      allMoons: List[moon],
      steps: Long,
      history: Set[List[(Int, Int)]]
  ): Long = {
    val temp = allMoons.map(x => x.toxTup)
    if (history(temp)) {
      allMoons.foreach(x => println(x.toxTup()))
      steps + 1
    } else cont(update(allMoons), steps + 1, history incl temp)
  }
  def cont2(
      allMoons: List[moon],
      steps: Int,
      history: Set[List[(Int, Int)]]
  ): Int = {
    val temp = allMoons.map(x => x.toyTup)
    if (history(temp)) steps + 1
    else cont2(update(allMoons), steps + 1, history incl temp)
  }
  def cont3(
      allMoons: List[moon],
      steps: Int,
      history: Set[List[(Int, Int)]]
  ): Int = {
    val temp = allMoons.map(x => x.tozTup)
    if (history(temp)) steps + 1
    else cont3(update(allMoons), steps + 1, history incl temp)
  }

  val r1 = math.BigInt(186027)
  val r2 = math.BigInt(135023)
  val r3 = math.BigInt(193051)
  r1 * r2 * r3
  (1 to 193051 / 2).filter(i => 193051 % i == 0)
  val r4 = r1 * r2 / r1.gcd(r2)
  r3 * r4 / r3.gcd(r4)

  cont(update(allMoons), 1, history1)
  cont2(update(allMoons), 1, history2)
  cont3(update(allMoons), 1, history3)
}
