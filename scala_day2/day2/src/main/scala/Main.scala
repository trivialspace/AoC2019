import scala.annotation.tailrec
object Main extends App {
  println("Hello, World!")

  val input: String = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,6,27,2,9,27,31,1,5,31,35,1,35,10,39,1,39,10,43,2,43,9,47,1,6,47,51,2,51,6,55,1,5,55,59,2,59,10,63,1,9,63,67,1,9,67,71,2,71,6,75,1,5,75,79,1,5,79,83,1,9,83,87,2,87,10,91,2,10,91,95,1,95,9,99,2,99,9,103,2,10,103,107,2,9,107,111,1,111,5,115,1,115,2,119,1,119,6,0,99,2,0,14,0"

  val myvec: Vector[Int] = input.split(",").map(_.trim()).toList.map(_.toInt).toVector

  val mytest: Vector[Int] = "2,4,4,5,99,0".split(",").map(_.trim()).toList.map(_.toInt).toVector
  

  def computeValue(position: Int, memory:Vector[Int]): Int={
    memory(position) match {
      case 1 => memory(memory(position+1)) + memory(memory(position+2))
      case 2 => memory(memory(position+1)) * memory(memory(position+2))
    }
  }

  def update(position:Int, memory:Vector[Int]):Vector[Int]={
  if (memory(position)==99){
    memory
  }  
  else {
    update(position+4, memory.updated(memory(position+3), computeValue(position, memory)))
  }
  }

  val myvec2 = myvec.updated(1, 12).updated(2,2)
  val mygen = new scala.util.Random()

  @scala.annotation.tailrec
  def try2(noun:Int, verb:Int):Int={
    println(noun)
    println(verb)
    if (update(0, myvec.updated(1,noun).updated(2,verb))(0)==19690720) 100*noun + verb else try2(mygen.nextInt(100), mygen.nextInt(100))
  }

}