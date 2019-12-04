object day4 {
    def check1(num:Int):Boolean={
        val pairs = num.toString.map(_.asDigit).sliding(2).toList
        val temp = (for {pair <- pairs}
            yield (pair(0)==pair(1), pair(0)<=pair(1))).unzip
        val consecutive = temp._1.sliding(3).toList//.map(e=>e(0).^(e(1))).toList

        (consecutive.map(e=>checkC(e)).exists(identity) || checkFirst(consecutive.head) || checkLast(consecutive.last)) && temp._2.forall(identity)
    }

    def checkC(triple:List[Boolean]):Boolean={
        triple==List(false, true, false)
    }

    def checkFirst(triple:List[Boolean]):Boolean={
        triple(0)==true && triple(1)==false
    }

    def checkLast(triple:List[Boolean]):Boolean={
        triple(1)==false && triple(2)==true
    }
    val result = for(i<-(234208 to 765869) if check1(i)) yield i

}