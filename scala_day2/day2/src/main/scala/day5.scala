object day5{
    val input = """3,225,1,225,6,6,1100,1,238,225,104,0,1102,88,66,225,101,8,125,224,101,-88,224,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1101,87,23,225,1102,17,10,224,101,-170,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1101,9,65,225,1101,57,74,225,1101,66,73,225,1101,22,37,224,101,-59,224,224,4,224,102,8,223,223,1001,224,1,224,1,223,224,223,1102,79,64,225,1001,130,82,224,101,-113,224,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1102,80,17,225,1101,32,31,225,1,65,40,224,1001,224,-32,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,2,99,69,224,1001,224,-4503,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1002,14,92,224,1001,224,-6072,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,102,33,74,224,1001,224,-2409,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,677,677,224,1002,223,2,223,1006,224,329,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,344,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,359,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,374,1001,223,1,223,8,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,404,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,434,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,449,101,1,223,223,107,677,226,224,1002,223,2,223,1006,224,464,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,479,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,494,1001,223,1,223,1108,677,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1107,677,226,224,1002,223,2,223,1005,224,524,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,539,101,1,223,223,1008,226,677,224,1002,223,2,223,1005,224,554,1001,223,1,223,7,226,677,224,1002,223,2,223,1005,224,569,101,1,223,223,1007,677,226,224,1002,223,2,223,1006,224,584,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,599,101,1,223,223,1007,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,644,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,659,101,1,223,223,8,226,226,224,1002,223,2,223,1005,224,674,101,1,223,223,4,223,99,226"""
    val myvec: Vector[Int] = input.split(",").map(_.trim()).toList.map(_.toInt).toVector

    // def update(position:Int, memory:Vector[Int]):Vector[Int]={
    //     if (memory(position)==99){
    //         memory
    //       }  
    //       else {
    //         val opcode = (memory(position)/100).toString().reverse.padTo[Char](3, '0')
    //         memory(position)%100 match{
    //             case 1 => {
    //                 val temp = memory.drop(position+1).take(3)
    //                 val temp2 = for ((char, value) <- opcode.zip(temp))
    //                     yield if (char.toString.toInt==1) value else memory(value)
    //                 update(position+4, memory.updated(temp(2), temp2(0)+temp2(1)))
    //             }
    //             case 2 => {
    //                 val temp = memory.drop(position+1).take(3)
    //                 val temp2 = for ((char, value) <- opcode.zip(temp))
    //                     yield if (char.toString.toInt==1) value else memory(value)
    //                 update(position+4, memory.updated(temp(2), temp2(0)*temp2(1)))
    //             }
    //             case 4 => {
    //                 println(memory(memory(position+1)))
    //                 update(position+2, memory)
    //             }
    //              case _ => {println(memory(position)%100)
    //                 println(position)
    //              memory}
    //         }
    //   }

    // }
    def two(myvec:Vector[Int], pos1:Int, pos2:Int, final_pos:Int):Vector[Int]={
        myvec.updated(final_pos, myvec(pos1)*myvec(pos2))
    }
    def one(myvec:Vector[Int], pos1:Int, pos2:Int, final_pos:Int):Vector[Int]={
        myvec.updated(final_pos, myvec(pos1)+myvec(pos2))
    }
    def three(myvec:Vector[Int], position:Int, input:Int):Vector[Int]={
        myvec.updated(position, input)
    }
    def four(myvec:Vector[Int], position:Int):Vector[Int]={
        println(myvec(position))
        myvec
    }
    def five(myvec:Vector[Int], current:Int, pos2:Int):Vector[Int]={
        myvec.updated(current, myvec(pos2))
    }
    def seven(myvec:Vector[Int], pos1:Int, pos2:Int, final_pos:Int):Vector[Int]={
        if (myvec(pos1)<myvec(pos2)) myvec.updated(final_pos, 1) else myvec.updated(final_pos, 0) 
    }
    def eight(myvec:Vector[Int], pos1:Int, pos2:Int, final_pos:Int):Vector[Int]={
        if (myvec(pos1)==myvec(pos2)) myvec.updated(final_pos, 1) else myvec.updated(final_pos,0)
    }
    def parser(opcode:Int):(Int, List[Int])={
        val operation = opcode%100
        (opcode%100,(opcode/100).toString.map(_.asDigit).reverse.toList)
    }
    def get_pos(mode:Option[Int], myvec:Vector[Int], position:Int):Int={
        val mymode = mode.getOrElse(0)
        mymode match{
            case 1 => position
            case 0 => myvec(position)
        }
    }
    //def get_params(params:List[Int])
    def update(myvec:Vector[Int], position:Int, input:Int=0):Vector[Int]={
        //val input:Int= 5
        val (opcode,params) = parser(myvec(position))
        // println("new run")
        // println(opcode)
        // println(myvec)
        // println(position)
        // println(params)
        opcode match{
            case 99 => myvec
            case 1  => update(one(myvec, get_pos(mode =params.lift(0), myvec, position+1), get_pos(mode=params.lift(1), myvec, position+2), get_pos(mode=params.lift(2), myvec, position+3)), position+4)
            case 2  => update(two(myvec, get_pos(mode =params.lift(0), myvec, position+1), get_pos(mode=params.lift(1), myvec, position+2), get_pos(mode=params.lift(2), myvec, position+3)), position+4)
            case 3  => update(three(myvec, get_pos(params.lift(0), myvec, position+1),input), position+2)
            case 4  => update(four(myvec, get_pos(params.lift(0), myvec, position+1)), position+2)
            case 5  => if (myvec(get_pos(params.lift(0), myvec, position+1))!=0) update(myvec, myvec(get_pos(params.lift(1), myvec, position+2))) else update(myvec, position+3)
            case 6  => if (myvec(get_pos(params.lift(0), myvec, position+1))==0) update(myvec, myvec(get_pos(params.lift(1), myvec, position+2))) else update(myvec, position+3)
            case 7  => update(seven(myvec, get_pos(params.lift(0), myvec, position+1), get_pos(params.lift(1), myvec, position+2), get_pos(params.lift(2), myvec, position+3)), position+4)
            case 8  => update(eight(myvec, get_pos(params.lift(0), myvec, position+1), get_pos(params.lift(1), myvec, position+2), get_pos(params.lift(2), myvec, position+3)), position+4)
        }

    }

    val temp = List(1,2,3)
    //val temp = if(1!=0) 1 else 0

    val small = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    val smallvec = small.split(",").map(_.trim()).toList.map(_.toInt).toVector
    update(smallvec, 0) 
    //val myvec = smallvec
    val position = 0
    update(myvec,0)
    val memory = smallvec
    val myvec2 = myvec.updated(myvec(225),1)
    // update(2, myvec2)

}