class DominantAHP(val m:Int, val n:Int, val matA:Matrix, val aBs:Map[Int, Matrix]) {

  val altNames = new Array[String](m)
  for(i <- 0 until m) altNames(i) = new String("alt." + i)
  val cNames = new Array[String](n)
  for(j <- 0 until n) cNames(j) = new String("criterion" + j)

  def setAltName(row:Int, name:String) {
    if( (0 until m).contains(row) ) altNames(row) = name
  }
  def setAltNames(names:Seq[String]) {
    for(i <- 0 until names.size) setAltName(i, names.apply(i))
  }
  def setCName(col:Int, name:String) {
    if( (0 until n).contains(col) ) cNames(col) = name
  }
  def setCNames(names:Seq[String]) {
    for(j <- 0 until names.size) setCName(j, names.apply(j))
  }

  val _bs:scala.collection.mutable.Map[Int, Matrix] = scala.collection.mutable.Map.empty
  for(d <- aBs.keySet) if(aBs.contains(d)) _bs += d -> aBs.apply(d).copy
  val bs:Map[Int, Matrix] = scala.util.Sorting.stableSort(_bs.toSeq, (a:(Int,Matrix), b:(Int,Matrix))=>a._1 < b._1).toMap

  def this(cm:Int, cn:Int) = this(cm, cn, new Matrix(cm, cn, 1.0), Map(0 -> new Matrix(cn, 1, 1.0)))

//  def this(aSPCM:SPCM, dIds:Array[Int]) = {
//    this(aSPCM.sizeA, aSPCM.sizeC, (new Matrix(aSPCM.sizeA, aSPCM.sizeC)), )
//  }

  val pcmOfA = new Array[Matrix](n)
  for(c <- 0 until n) {
    pcmOfA(c) = new Matrix(m,m,1.0)
    for(i <- 0 until m; j <- 0 until m) pcmOfA(c).set(i,j, matA.at(i,c)/matA.at(j,c))
  }

  val pcmOfBs:scala.collection.mutable.Map[Int, Matrix] = scala.collection.mutable.Map.empty
  for(d <- bs.keySet) {
    var mat = new Matrix(n, n, 1.0)
    for(i <- 0 until n; j <- 0 until n) mat.set(i,j, bs.apply(d).at(i,0)/bs.apply(d).at(j,0))
    pcmOfBs.put(d, mat)						
  }

  val ciOfA = new Matrix(n, 0, 0.0)
  val ciOfBs = new Matrix(m, 0, 0.0)
  def recalculateFromPCM {
    // for A
    for(c <- 0 until n) {
      var tmpPowA = pcmOfA(c).powerMethod
      ciOfA.set(c, 0, tmpPowA._1)
      for(i <- 0 until m) matA.set(i,c, tmpPowA._2.at(i,0))
      println("CI of matrix A at " + c +" : "+ funcCI(tmpPowA._1, m) + "  (eigenvalue: "+tmpPowA._1+")")
    }

    // for Bs
    for(d <- bs.keySet) {
      var tmpPowB = pcmOfBs.apply(d).powerMethod
      ciOfBs.set(d, 0, tmpPowB._1)
      tmpPowB._2.copyTo(bs.apply(d))
      println("CI of weight vector b when " + altNames(d) + " is dominant : "+funcCI(tmpPowB._1, n)+ "  (eigenvalue: "+tmpPowB._1+")")
      println("")
    }
  }

  def funcCI(lambda:Double, aN:Int):Double = {
    (lambda-aN*1.0)/(aN*1.0-1.0)
  }

  override def toString:String = {
    var str = "":String
    str = str + "代替案("+m+"個): "
    altNames.map(s => str=str+"   "+s)
    str = str + "\n評価基準("+n+"個): "
    cNames.map(s => str=str+"   "+s)
    str = str + "\n視点(支配代替案)("+bs.size+"個): "
    bs.keySet.map(d => str = str + "    " + altNames(d))

    str = str + "\n\n"
    str = str + "評価行列 Evaluation Matrix A:\n" + matA.toString + "\n"
    for(d <- bs.keySet) {
      str = str + "支配代替案が " + altNames(d) + " のときの重みベクトル\n"
      str = str + bs.apply(d)
    }
    str += "\n"
    str += "一対比較行列 Pairwise comparison matrixes\n"
    str += "評価行列A作成用\n"
    for(cri <- 0 until n) {
      str += "評価行列Aの評価基準 " + cNames(cri) + " についての一対比較行列\n"
      //str += pcmOfA(cri).toString
      str += pcmOfA(cri).toStringR
    }
    str += "評価基準の重みベクトル作成用\n"
    for(d <- pcmOfBs.keySet) {
      str += "支配代替案が " + altNames(d) + " のときの重みベクトルの一対比較行列\n"
      //str += pcmOfBs(d).toString
      str += pcmOfBs(d).toStringR
    }
    str
  }

  def aggregateScore(d:Int) : Matrix = {
    require( bs.keySet.contains(d) )
    var x = matA * matA.rowDiagInverseMatrix(d) * bs.apply(d)
    x.normalizeColumn(0)
    x
  }

  def aggregateScore(d:Int, i:Int) : Matrix = {
    require( bs.keySet.contains(d) )
    require( (0 until m).contains(i) )

    var x = aggregateScore(d)
    x / x.at(i,0)
    x
  }

  def ccm : DominantAHP = {
    var betas:Map[Int, Matrix] = Map()
    var preBetas:Map[Int, Matrix] = Map()

    for(d <- bs.keySet) {
      betas = betas + (d -> bs.apply(d).copy)
      preBetas = preBetas + (d -> bs.apply(d).copy)
    }

    var count:Int = 0
    var enoughToClose = true
    do {
      for(d <- betas.keySet) {
	betas.apply(d).copyTo(preBetas.apply(d))
      }

      for(d <- betas.keySet) {
	betas.apply(d).fill(0.0)
	for(dd <- betas.keySet) {
	  val mat_dd = matA.rowDiagMatrix(d) * matA.rowDiagInverseMatrix(dd) * preBetas.apply(dd)
	  mat_dd.normalizeColumn(0)
	  betas.apply(d).setAdd(mat_dd, betas.apply(d))
	}
	betas.apply(d) / betas.size
      }
      count = count + 1

      enoughToClose = true
      for(d <- betas.keySet) {
	enoughToClose = enoughToClose && (betas.apply(d) == preBetas.apply(d))
      }

    }while( (! enoughToClose) && (count < 10) )
    println("CCM iteration is " + count)

    new DominantAHP(m, n, matA, betas)
  }

  def geomMeanCCM : DominantAHP = {
    var betas:Map[Int, Matrix] = Map()
    var newBs:Map[Int, Matrix] = Map()

    for(d <- bs.keySet) {
      betas = betas + (d -> bs.apply(d).copy)
    }

    for(d <- betas.keySet) {
      val newB = new Matrix(n, 1)
      for(j <- 0 until matA.n) {
	var mul = 1.0
	for(k <- betas.keySet) {
	  mul = mul * matA.at(d, j)/ matA.at(k,j) * betas.apply(k).at(j, 0)
	}
	newB.set(j,0, Math.pow(mul, 1.0/bs.keySet.size))
      }
      newB.normalizeColumn(0)
      newBs = newBs + (d -> newB)
    }

    new DominantAHP(m, n, matA, newBs)
  }


  def spcm:SPCM = {
    var matR = new SPCM(m, n)
    for(c <- 0 until n) {
      for(i <- 0 until m; j <- 0 until m) matR.set(i,c, j,c, pcmOfA(c).at(i,j))
    }
    for(d <- pcmOfBs.keySet) {
      for(i <- 0 until n; j <- 0 until n) {
	matR.set(d,i, d,j, pcmOfBs.apply(d).at(i,j))
	//println(d,i, d,j, pcmOfBs.apply(d).at(i,j))
      }
    }
    matR
  }

//def spcm2
//  def this(aSPCM:SPCM, dIds:Array[Int], aAltNames:Array[String], aCNames:Array[String]) = 

  // output to file
  def outputToFile(filename:String) {
    import scala.io.Source
    import java.io.PrintWriter
    
    val out = new PrintWriter(filename)
    out.println(m)
    out.println(n)
    altNames.foreach(out.println(_))
    cNames.foreach(out.println(_))
    for(cr <- 0 until n) {
      for(i <- 0 until m; j <- 0 until m) out.println(pcmOfA(cr).at(i,j))
    }
    out.println(bs.size)
    for(d <- bs.keySet) {
      out.println(d)
      for(i <- 0 until n; j <- 0 until n) out.println(pcmOfBs(d).at(i,j))
    }
    out.close
  }  
}



  


