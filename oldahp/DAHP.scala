class DominantAHP(val m:Int, val n:Int) {

  val matA = new Matrix(m, n, 1.0)
  val bs:scala.collection.mutable.Map[Int, Matrix] = scala.collection.mutable.Map.empty

  val altNames = new Array[String](m)
  for(i <- 0 until m) altNames(i) = new String("alt." + (i+1))
  val cNames = new Array[String](n)
  for(j <- 0 until n) cNames(j) = new String("criterion" + (j+1))

  val pcmOfA = new Array[Matrix](n)
  for(c <- 0 until n) {
    pcmOfA(c) = new Matrix(m,m,1.0)
  }
  val pcmOfBs:scala.collection.mutable.Map[Int, Matrix] = scala.collection.mutable.Map.empty


  var title = new String("Dominant AHP")

  def this(aTitle:String, aM:Int, aN:Int) {
    this(aM, aN)
    setTitle(aTitle)
  }
  def this(aM:Int, aN:Int, aMatA:Matrix, aBs:Map[Int, Matrix]) {
    this(aM, aN)

    setMatA(aMatA)
    for(c <- 0 until n) {
      val mat = new Matrix(m,m, 1.0)
      for(i <- 0 until m; j <- 0 until m) mat.set(i,j, matA.at(i,c)/matA.at(j,c))
      setPCMofA(c, mat)
    }

    for(d <- aBs.keySet) {
      setB(d, aBs.apply(d).copy)
      val mat = new Matrix(n, n, 1.0)
      for(i <- 0 until n; j <- 0 until n) mat.set(i,j, bs.apply(d).at(i,0)/bs.apply(d).at(j,0))
      setPCMofB(d, mat)
    }
  }


  def copyWithoutPCMs: DominantAHP = {
    val ahp = new DominantAHP(m, n)
    ahp.setMatA(matA)
    for(d <- bs.keySet) {
      ahp.setB(d, bs.apply(d))
    }
    ahp.setAltNames(altNames)
    ahp.setCNames(cNames)
    ahp.setTitle(title)

    ahp
  }
  def copy:DominantAHP = {
    val ahp = copyWithoutPCMs
    ahp.setPCMofA(pcmOfA)
    ahp.setPCMofBs(pcmOfBs)

    ahp
  }

  def setTitle(aTitle:String) {
    title = new String(aTitle)
  }

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

  def setMatA(aMatA: Matrix) {
    aMatA.copyTo(matA)
  }


  def setPCMofA(aPCM:Array[Matrix]) {
    for(c <- 0 until n) {
      setPCMofA(c, aPCM(c))
    }
  }
  def setPCMofA(c:Int, aPCM:Matrix) {
    if( (0 until n) contains c ) {
      aPCM.copyTo(pcmOfA(c))
    }
  }

  def setB(d:Int, aB:Matrix) {
    if( (0 until m) contains d ) {
      bs.put(d, aB.copy)
    }
  }
  def setBs(aBs:Map[Int, Matrix]) {
    for(d <- aBs.keySet) {
      bs.put(d, aBs.apply(d).copy)
    }
  }


  def setPCMofBs(aPCM:scala.collection.mutable.Map[Int, Matrix]) {
    for(d <- aPCM.keySet) {
      setPCMofB(d, aPCM.apply(d))
    }
  }
  def setPCMofB(d:Int, aPCM:Matrix) {
    if( (0 until m) contains d) {
      pcmOfBs.put(d, aPCM.copy) 
    }
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
    bs.clear
    for(d <- pcmOfBs.keySet) {
      var tmpPowB = pcmOfBs.apply(d).powerMethod
      ciOfBs.set(d, 0, tmpPowB._1)
      setB(d, tmpPowB._2)
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
    //bs.keySet.map(d => str = str + "    " + altNames(d))
    bs.keySet.toList.sorted.map(d => str = str + "    " + altNames(d))

    str = str + "\n\n"
    str = str + "評価行列 Evaluation Matrix A:\n" + matA.toString + "\n"
    for(d <- bs.keySet.toList.sorted) {
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
    for(d <- pcmOfBs.keySet.toList.sorted) {
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

    val result = copy
    result.setBs(betas)

    result
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

    val result = copy
    result.setBs(newBs)

    result
  }


  def spcm:SPCM = {
    var matR = new SPCM(m, n)
    for(c <- 0 until n) {
      for(i <- 0 until m; j <- 0 until m) matR.set(i,c, j,c, pcmOfA(c).at(i,j))
    }
    for(d <- pcmOfBs.keySet) {
      for(i <- 0 until n; j <- 0 until n) {
	matR.set(d,i, d,j, pcmOfBs.apply(d).at(i,j))
      }
    }
    matR
  }

//  def setFromSPCM(aSPCM:SPCM, dSet:scala.collection.mutable.Set[Int]) {
  def setFromSPCM(aSPCM:SPCM, dSet:scala.collection.Set[Int]) {
    for(c <- 0 until n) {
      val tmpA = new Matrix(m,m,1.0)
      for(i <- 0 until m; j <- 0 until m) tmpA.set(i,j, aSPCM.get(i,c, j,c))
      for(i <- 0 until tmpA.m) tmpA.set(i,i,1.0)
      setPCMofA(c, tmpA)
    }
    for(d <- dSet) {
      val tmpB = new Matrix(n,n,1.0)
      for(i <- 0 until n; j <- 0 until n) {
	tmpB.set(i,j, aSPCM.get(d,i, d,j))
      }
      for(i <- 0 until tmpB.m) tmpB.set(i,i, 1.0)
      setPCMofB(d,tmpB)
    }
    recalculateFromPCM
  }

}

object DAHPUtil {
  import org.apache.poi.hssf.usermodel._
  import java.io.FileOutputStream
  import java.io.FileInputStream
  import java.io.File

  def writeMatrix2Sheet(mat:Matrix, sheet:HSSFSheet) {
    for(i <- 0 until mat.m) {
      val row = sheet.createRow(i)
      for(j <- 0 until mat.n) {
	val cell = row.createCell(j)
	cell.setCellValue(mat.at(i,j))
      }
    }    
  }

  def setMatrixFromSheet(mat:Matrix, sheet:HSSFSheet) {
    for(i <- 0 until mat.m) {
      for(j <- 0 until mat.n) {
	mat.set(i,j, sheet.getRow(i).getCell(j).getNumericCellValue())
      }
    }
  }

  def readExcelFile(filename:String):Either[Exception, DominantAHP] = {
    readExcelFile(new File(filename))
  }
  def readExcelFile(file:File):Either[Exception, DominantAHP] = {
    var iStream:FileInputStream = null
    try {
      iStream = new FileInputStream(file)
      val book = new HSSFWorkbook(iStream)
      val specSheet = book.getSheet("spec")
      val aM = specSheet.getRow(0).getCell(0).getNumericCellValue().toInt
      val aN = specSheet.getRow(0).getCell(1).getNumericCellValue().toInt
      val aDAHP = new DominantAHP(aM, aN)
      val aDNum = specSheet.getRow(0).getCell(2).getNumericCellValue().toInt
      var ds:Set[Int] = Set.empty
      for(i <- 0 until aDNum) {
	ds += specSheet.getRow(1).getCell(i).getNumericCellValue().toInt
      }
      val aNames = new Array[String](aM)
      for(i <- 0 until aM){
	aNames(i) = specSheet.getRow(2).getCell(i).getStringCellValue()
      }
      aDAHP.setAltNames(aNames)
      val cNames = new Array[String](aN)
      for(i <- 0 until aN){
	cNames(i) = specSheet.getRow(3).getCell(i).getStringCellValue()
      }
      aDAHP.setCNames(cNames)
      val spcmSheet = book.getSheet("spcm")
      val aSPCM = aDAHP.spcm
      setMatrixFromSheet(aSPCM, spcmSheet)
      aDAHP.setFromSPCM(aSPCM, ds)
      return Right(aDAHP)
    }
    catch {
//      case es:java.lang.IllegalStateException => Left(es)
//      case ei:java.io.IOException => Left(ei) 
      case e:Exception => {
	e.printStackTrace
	println(e)
	return Left(e)
      }
    }
    finally {
      if(iStream != null) iStream.close()
    }
    Left(new Exception("Cannot read File " + file))
  }

  def writeExcelFile(dahp:DominantAHP, filename:String):Either[Exception, String] = {
    writeExcelFile(dahp, new File(filename))
  }
  def writeExcelFile(dahp:DominantAHP, file:File):Either[Exception, String] = {

    var oStream: FileOutputStream = null
    try {
      var count = 0

      val book = new HSSFWorkbook

      //spec sheet
      val specSheet = book.createSheet("spec")
      val specRow = specSheet.createRow(0)
      val mCell = specRow.createCell(0)
      val nCell = specRow.createCell(1)
      val dCell = specRow.createCell(2)
      mCell.setCellValue(dahp.m)
      nCell.setCellValue(dahp.n)
      dCell.setCellValue(dahp.bs.keySet.size)

      val dRow = specSheet.createRow(1)
      count = 0
      for(d <- dahp.bs.keySet.toList.sorted) {
	val tmpDCell = dRow.createCell(count)
	tmpDCell.setCellValue(d)
	count = count + 1
      }
      
      val altNameRow = specSheet.createRow(2)
      count = 0
      for(i <- 0 until dahp.m) {
	val aCell = altNameRow.createCell(count)
	aCell.setCellValue(dahp.altNames(i))
	count = count + 1
      }
      val crNameRow = specSheet.createRow(3)
      count = 0
      for(j <- 0 until dahp.n) {
	val cCell = crNameRow.createCell(count)
	cCell.setCellValue(dahp.cNames(j))
	count = count + 1
      }

      //spcm
      val spcmSheet = book.createSheet("spcm")
      writeMatrix2Sheet(dahp.spcm, spcmSheet)

      oStream = new FileOutputStream(file, false) // append = false
      book.write(oStream)
      return Right(file.toString)
    }
    catch {
      case e:Exception => return Left(e)
    }
    finally {
      if(oStream != null ) oStream.close
    }
    Left(new Exception("Cannot write to file :" + file))
  }

}
