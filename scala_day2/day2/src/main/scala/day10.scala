import cats.instances.`package`.boolean
import scala.collection.mutable
object day10{
    val input = """.###..#......###..#...#
    #.#..#.##..###..#...#.#
    #.#.#.##.#..##.#.###.##
    .#..#...####.#.##..##..
    #.###.#.####.##.#######
    ..#######..##..##.#.###
    .##.#...##.##.####..###
    ....####.####.#########
    #.########.#...##.####.
    .#.#..#.#.#.#.##.###.##
    #..#.#..##...#..#.####.
    .###.#.#...###....###..
    ###..#.###..###.#.###.#
    ...###.##.#.##.#...#..#
    #......#.#.##..#...#.#.
    ###.##.#..##...#..#.#.#
    ###..###..##.##..##.###
    ###.###.####....######.
    .###.#####.#.#.#.#####.
    ##.#.###.###.##.##..##.
    ##.#..#..#..#.####.#.#.
    .#.#.#.##.##########..#
    #####.##......#.#.####."""
    // class ast(x:Int, y:Int){
    //     def slope(x_pos:Int, y_pos:Int):Double={
    //         (y_pos - y)/(x_pos - x)
    //     }
    // }

    val sInput = """.#..#
    .....
    #####
    ....#
    ...##"""
    val sfield = sInput.split("\\n").map(_.trim().toList)
    val field = input.split("\\n").map(_.trim().toList)
    val asteroids = (for (i <- Range(0,field.length); j <- Range(0,field(0).length))
    yield if (field(i)(j)=='#') (i,j) else (-1,-1)).filter(_!=(-1,-1)).toList
    
    def getAsteroids(input:String):List[(Int, Int)]={
        val field = input.split("\\n").map(_.trim().toList)
        (for (i <- Range(0,field.length); j <- Range(0,field(0).length))
    yield if (field(i)(j)=='#') (i,j) else (-1,-1)).filter(_!=(-1,-1)).toList
    }


    def viz(x:Int, y:Int, smallAsteroids:List[(Int,Int)]):Int={
        (for ((a,b) <- smallAsteroids) yield ((b-y).toFloat/(a-x), a>x, b>y) ).toSet.size
    }

    def counter(asteroids:List[(Int,Int)], x:Int, y:Int):Int={
        val noVert = viz(x,y, asteroids.filter(_._1!=x))
        val allVert = asteroids.filter(_._1==x).map({case (a,b)=>scala.math.signum(scala.math.subtractExact(b, y))}).toSet.size 
        noVert + allVert
    }
    
    val position = (for ((a,b) <- asteroids)
        yield ((a,b),counter(asteroids, a, b))).maxBy(_._2)._1

    def viz2(x:Int, y:Int, smallAsteroids:List[(Int,Int)]):List[(Float, (Int, Int))]={
        (for ((a,b) <- smallAsteroids) yield ((b-y).toFloat/(a-x), (a-x,b-y) ))
    }

    def viz3(x:Int, y:Int, smallAsteroids:List[(Int,Int)]):Map[Float, List[(Int, Int)]]={
        (for ((a,b) <- smallAsteroids) yield ((b-y).toFloat/(a-x)->(a-x,b-y) )).groupMap(_._1)(_._2)
    }
    val temp0 = viz2(position._1, position._2, asteroids)
    val temp = viz3(position._1, position._2, asteroids)
    val temp2 = (for((k,v)<-temp)yield (k,v.sortBy(_._1)))
    val right = scala.collection.immutable.List(temp2.filter({case (_, b) =>((b.head._1>0)|| (b.head._1==0 && b.head._2>0))}).toSeq.sortWith(_._1>_._1):_*)
    val left = scala.collection.immutable.List(temp2.filter({case (_, b) =>((b.head._1<0)|| (b.head._1==0 && b.head._2<0))}).toSeq.sortWith(_._1<_._1):_*)
    val full = right ::: left
    full.size
    full.filter({ case(f,l)=> l.length>1}).drop(0).head
    def nrAtLength(mylen:Int, full:List[(Float, List[(Int, Int)])]):Int={
        full.filter({ case(f,l)=> l.length==mylen}).size
    }
    nrAtLength(1,full)

    asteroids.filter(_._1==position._1)
    full.foldLeft(0)({case(a,(b,d))=> a+d.size})
    temp0.size
    asteroids.size
    temp0.map({case (a,(b,c))=>a}).toSet.size
    temp0.filter({case (a,(b,c))=>a>10})
}
//.filter({case (a,b)=>(a!=position._1) || (b!=position._2)}))