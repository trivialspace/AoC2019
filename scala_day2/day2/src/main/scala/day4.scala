object day4 {
    def check1(num:Int):Boolean={
        val pairs = num.toString.map(_.asDigit).sliding(3).toList
        val temp = (for {pair <- pairs}
            yield (check3(pair), (pair(0)<=pair(1)) && (pair(1)<=pair(2)))).unzip
        temp._1.exists(identity) && temp._2.forall(identity)
    }
    def check3(triple:IndexedSeq[Int]):Boolean={
        if (triple(0)==triple(1) && triple(1)==triple(2)) false else triple(0)==triple(1) || triple(1)==triple(2)
    }
    val result = for(i<-(234208 to 765869) if check1(i)) yield i

}