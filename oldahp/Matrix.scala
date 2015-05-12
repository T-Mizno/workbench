class Rational(val nume: Int, val deno:Int) {
  require(deno != 0)

  override def toString:String = {
    val totalLength = 4
    val denoLength = deno.toString.length
    val numeLength = totalLength - denoLength -1
    if (deno == 1) ("%"+totalLength+"d").format(nume)
    else ("%"+numeLength+"d").format(nume) + "/" +("%"+denoLength+"d").format(deno)
  }

  def this(aNum:Int) = this(aNum, 1)

  def inverse:Rational = {
    require(nume != 0)
    new Rational(deno, nume)
  }
}

class Matrix(val m:Int, val n:Int, val aDefault:Double) {
  val _values = Array.ofDim[Double] (m, n)
  var _default = aDefault

  fill(_default)

  val epsilon = 0.0001

  def this(cm:Int, cn:Int) = this(cm, cn, 0.0)
  def this(cm:Int, cn:Int, cDefault:Double, cValues:Seq[((Int,Int), Double)]) = {
    this(cm, cn, cDefault)
    for (v <- cValues) {
      v match {
       case ((i:Int, j:Int), v:Double) => set(i,j,v)
      }
    }
  }
  def this(cm:Int, cn:Int, cValues : Seq[((Int,Int), Double)]) = this(cm,cn, 0.0, cValues)

  def this(cm:Int, cn:Int, cValues:List[Double]) = {
    this(cm,cn)
    require(cValues.length >= cm*cn)
    for(i <- 0 to (m-1); j <- 0 to (n-1)) set(i,j, cValues(i*cn + j))
  }

  def _toString(elem2Str: Double => String):String = {
    var str = ""
    for(i <- 0 until m) {
      str = str + "  " + elem2Str(at(i,0))
      for(j <- 1 until n) {
	str = str + ", " + elem2Str(at(i,j))
      }
      str = str + "\n"
    }
    str
  }

  override def toString:String = _toString("%4f".format(_))
  def toStringR:String = _toString(double2Rational(_).toString)

  def double2Rational(x:Double):Rational = {
    val numSet:List[Int] = (1 to 21).toList
    var result = new Rational(1)
    var diff = 1000000000.0

    for(aN <- 0::numSet) {
      if(near(x, aN.toDouble)) return new Rational(aN)

      val tmpDiff = Math.abs(x - aN.toDouble)
      if( tmpDiff < diff) {
	diff = tmpDiff
	result = new Rational(aN)
      }
    }

    for(aN <- numSet) {
      if(near(x, 1.toDouble/aN.toDouble)) return new Rational(1, aN)

      val tmpDiff = Math.abs(x - (1.toDouble/aN.toDouble))
      if( tmpDiff < diff) {
	diff = tmpDiff
	result = new Rational(1, aN)
      }
    }
    //println(x +"  "+result)
    result
  }

  def at(i:Int, j:Int):Double = _values(i)(j)
  def set(i:Int, j:Int, v:Double) {
    if ( (0 to (m-1)).exists(_ == i) && (0 to (n-1)).exists(_ == j) ) _values(i)(j) = v
  }

  def setPartial(si:Int, sj:Int, mat:Matrix) {
    for(i <- 0 until mat.m; j <- 0 until mat.n) {
      val di = si+i
      val dj = sj+j
      if((0 until m).contains(di) && (0 until n).contains(dj) )	{
	set(di,dj, mat.at(i,j))
      }
    }
  }

  def fill(v:Double) {
      for(i <- 0 until m; j <- 0 until n) set(i,j, v)
    }

  def +(that:Matrix):Matrix = {
    var mat = new Matrix(m, n)
    setAdd(that, mat)
    mat
  }
  def -(that:Matrix):Matrix = {
    var mat = new Matrix(m, n)
    setSub(that, mat)
    mat
  }
  def *(that:Matrix):Matrix = {
    var mat = new Matrix(m, that.n)
    setMultiply(that, mat)
    mat
  }

  def setAdd(that:Matrix, target:Matrix) {
    require( (that.m == m) && (that.n == n) )
    require( (target.m >= m) && (target.n >= n) )

    for(i <- 0 to (m-1); j <- 0 to (n-1)) target.set(i,j, at(i,j)+that.at(i,j))
  }
  def setSub(that:Matrix, target:Matrix) {
    require( (that.m == m) && (that.n == n) )
    require( (target.m >= m) && (target.n >= n) )

    for(i <- 0 to (m-1); j <- 0 to (n-1)) target.set(i,j, at(i,j)-that.at(i,j))
  }
  def setMultiply(that:Matrix, target:Matrix) {
    require( n == that.m )
    require( (target.m >= m) && (target.n >= that.n) )

    for(i <- 0 until m) {
      for(j <- 0 until that.n) {
        var inner = 0.0:Double
        for(k <- 0 until n) {
          inner = inner + at(i,k) * that.at(k,j)
        }
        target.set(i,j, inner)
     }
    }
  }

  def near(x:Double, y:Double):Boolean = Math.abs(x - y) < epsilon

  def ==(that:Matrix):Boolean = equals(that)
  def equals(that:Matrix):Boolean = {
    (m == that.m) && (n == that.n) && near(error(that), 0.0)
  }

  def copy:Matrix =  {
    var mat = new Matrix(m, n)
    copyTo(mat)
    mat
  }
  def copyTo(that:Matrix)  {
    require(m <= that.m  &&  n <= that.n)
    for(i <- 0 until m; j <- 0 until n) that.set(i,j, at(i,j))
  }

  def error(that:Matrix):Double = {
    Math.sqrt((0.0 /: (for(i <- 0 until m; j <- 0 until n) yield Math.pow(at(i,j)-that.at(i,j), 2.0))) (_ + _))
  }

  def divColumn(j:Int, deno:Double) {
    for(i <- 0 until m) set(i,j, at(i,j)/deno)
  }
  def /(deno:Double) {
    for(j <- 0 until n) divColumn(j, deno)
  }

  def sumOfColumn(j:Int):Double = {
    (0.0 /: column(j)) (_ + _)
  }

  def normalizeColumn(j:Int) {
    divColumn(j, sumOfColumn(j))
  }

  def powerMethod:(Double, Matrix) = {
    require(n == m)
    var x = new Matrix(m, 1, 1.0)
    var preX = new Matrix(m, 1, 1.0)

    //for(i <- 0 until m) x.set(i,0, at(i,0))
    for(i <- 0 until m) x.set(i,0, 1.0)

    var count:Int = 0
    do {
      x.copyTo(preX)
      setMultiply(preX, x)
      x.normalizeColumn(0)
      count = count + 1
    }while( (! x.equals(preX)) && (count < 100) )
    //}while( (! x.equals(preX)) && (count < 12) )
    println("power method itr: " + count)
    setMultiply(x, preX)
    (preX.at(0,0)/x.at(0,0), x)
  }

  def geomMeanMethod:(Double, Matrix) = {
    require(n == m)
    var x = new Matrix(m, 1, 1.0)

    for(i <- 0 until m) {
      x.set(i,0, Math.pow( (1.0 /: row(i))(_ * _), 1.0/m))

    }
    x.normalizeColumn(0)

    var tmpX = new Matrix(m,1)
    setMultiply(x, tmpX)

    ( ((0.0 /: (for(i <- 0 until m) yield tmpX.at(i,0)/x.at(i,0))) (_ + _))/m, x)
  }

  def powerMethod2:(Double, Matrix) = {
    require(n == m)
    var x = new Matrix(m, 1, 1.0)
    var preX = new Matrix(m, 1, 1.0)

    for(i <- 0 until m) x.set(i,0, at(i,0))
    //for(i <- 0 until m) x.set(i,0, 1.0)

    var count:Int = 0
    do {
      x.copyTo(preX)
      setMultiply(preX, x)
      x.normalizeColumn(0)
      count = count + 1
    }while( count < 12)

    println("power method itr: " + count)
    setMultiply(x, preX)
    (preX.at(0,0)/x.at(0,0), x)
  }

  def row(i:Int):Seq[Double] = {
    for(j <- 0 until n) yield at(i,j)
  }
  def column(j:Int):Seq[Double] = {
    for(i <- 0 until m) yield at(i,j)
  }

  def rowDiagMatrix(i:Int):Matrix = {
    var mat = new Matrix(n, n, 0.0)
    for(j <- 0 until n) mat.set(j,j, at(i,j))
    mat
  }
  def rowDiagInverseMatrix(i:Int):Matrix = {
    var mat = new Matrix(n, n, 0.0)
    for(j <- 0 until n) mat.set(j,j, 1.0/at(i,j))
    mat
  }
  def normalizedByDiagInverse(i:Int) {
    if((0 until m).contains(i)) {
      val result = this * rowDiagInverseMatrix(i)
      result.copyTo(this)
    }
  }


  def transpose : Matrix = {
    val matT = new Matrix(n, m)
    for(i <- 0 until matT.m; j <- 0 until matT.n) {
      matT.set(i,j, at(j, i))
    }
    matT
  }


}


class SPCM(val sizeA:Int, val sizeC:Int, var idx:(Int, Int) => Int) extends Matrix(sizeA*sizeC,sizeA*sizeC,0.0) {

  def this(cm:Int, cn:Int) = this(cm, cn, (i,j) => i + j*cm)

  override def toString = toStringR

  def set(a1:Int, c1:Int, a2:Int, c2:Int, v:Double) {
    set(idx(a1,c1), idx(a2,c2), v)
  }

  def get(a1:Int, c1:Int, a2:Int, c2:Int):Double =  {
    at(idx(a1,c1), idx(a2,c2))
  }

  def matHarker {
    for(i <- 0 until m) {
      val numOfZero = row(i).count(near(_, 0.0))
      //set(i,i, numOfZero+at(i,i))
      set(i,i, numOfZero+1.0)
    }
  }

  def aggregateScore(vec:Matrix):Matrix = {
    require(vec.m >= sizeA * sizeC)

    var x = new Matrix(sizeA, 1)
    for(i <- 0 until sizeA) {
      var score = 0.0:Double
      for(j <- 0 until sizeC){
	score = score + vec.at(idx(i,j),0)
      }
      x.set(i,0, score)
    }
    x.normalizeColumn(0)
    x
  }

  def aggregateScore(vec:Matrix, pivotI:Int):Matrix = {
    require(vec.m >= sizeA * sizeC)

    var x = aggregateScore(vec)
    val pivot = x.at(pivotI, 0)
    x / pivot
    x
  }

  def lls: Matrix = {
    val rows = scala.collection.mutable.Set.empty[(Matrix, Double)]
    for(i <- 0 until m; j <- 0 until n) {
      if((! near(at(i,j), 0.0)) ) {
	val matRow = new Matrix(1,n, 0.0)
	matRow.set(0, i, matRow.at(0,i) + 1.0)
	matRow.set(0, j, matRow.at(0,j) - 1.0)
	rows.add((matRow, Math.log(at(i,j))))
	//rows.add((matRow, at(i,j)))
      }
    }
    val matData = new Matrix(rows.size, n)
    val vecData = new Matrix(rows.size, 1)
    var rowCount = 0
    for((r,d) <- rows) {
      for(j <- 0 until n) {
	matData.set(rowCount, j, r.at(0,j))
      }
      vecData.set(rowCount, 0, d)
      rowCount = rowCount + 1
    }
    println(matData)
    println(vecData)
    val vec = MathUtil.leastSquare(matData, vecData)
    for(i <- 0 until vec.m) vec.set(i,0, Math.exp(vec.at(i,0)))
    vec
  }
  def countNonZeroElements : Int = {
    var countZero = 0:Int
    for(i <- 0 until m; j <- 0 until n) {
      if(near(at(i,j), 0.0)) countZero = countZero + 1
    }
    m * n - countZero
  }
}

object MathUtil {
  import org.apache.commons.math3.linear._

  def leastSquare(matA: Matrix, b:Matrix): Matrix = {
    val At = matA.transpose
    val AtA = At * matA
    val Atb = At * b
    println(AtA)
    println(Atb)
    println(gauss(AtA, Atb))
    gauss(AtA, Atb)
  }

  def gauss(matA: Matrix, b:Matrix):Matrix = {
    val aMatA = MatrixUtils.createRealMatrix(matA.m, matA.n)
    for(i <- 0 until matA.m; j <- 0 until matA.n) aMatA.setEntry(i,j, matA.at(i,j))
    val aVecb = aMatA.getRowVector(0)
    for(i <- 0 until matA.m) aVecb.setEntry(i, b.at(i,0))

    val solver = (new SingularValueDecomposition(aMatA)).getSolver
    val solution = solver.solve(aVecb)

    val x = new Matrix(matA.n, 1)
    for(j <- 0 until matA.n) x.set(j,0, solution.getEntry(j))
    x
//    val bind = bindMatrix(matA, b)
//    forward(bind)
//    backward(bind)
  }

  def bindMatrix(matA: Matrix, b:Matrix):Matrix = {
    require(matA.m == b.m)
    val matC = new Matrix(matA.m, (matA.n+b.n))
    for(i <- 0 until matC.m) {
      for(j <- 0 until matA.n) {
	matC.set(i,j, matA.at(i,j))
      }
      for(j <- 0 until b.n) {
	matC.set(i, j+matA.n, b.at(i,j))
      }
    }
    matC
  }

  def forward(mat: Matrix) {
    for(i <- 0 until mat.m) {
      for(ii <- (i+1) until mat.m) {
	for(j <- (i+1) until mat.n) {
	  mat.set(ii,j, mat.at(ii,j) - mat.at(ii, i)/mat.at(i,i) * mat.at(i,j))
	}
      }
    }
  }
  def backward(mat: Matrix):Matrix = {
    val x = new Matrix(mat.m, 1)
    for(i <- ( 0 until mat.m).reverse) {
      var tmpX = 0.0
      for(k <- (i+1) until (mat.n-1)) {
	tmpX = tmpX + mat.at(i,k) * x.at(k,0)
      }
      x.set(i,0, (mat.at(i,mat.n-1) - tmpX)/mat.at(i,i))
    }
    x
  }

}

object MatrixTest {
  val matA20 = new Matrix(3, 3, List(
    2.0, 1.0, 1.0,
    4.0, 1.0, 0.0,
    -2.0, 2.0, 1.0
    ))
  val matA58 = new Matrix(3, 4, List(
    1.0, 3.0, 3.0, 2.0,
    2.0, 6.0, 9.0, 5.0,
    -1.0, -3.0, 3.0, 0.0
    ))
  val vecb20 = new Matrix(3, 1, List(1.0, -2.0, 7.0))

  val matATest1 = new Matrix(8,8, List(
    0.01,	0.02,	0.03,	0.04,	0,	0,	0,	0,
    2,	1,	0,	3,	5,	0,	0,	0,
    0.7,	2,	1,	2,	3,	5,	0,	0,
    1.3,	1.69,	2,	1,	2,	3,	5,	0,
    0.17,	0.0289,	0.004913,	2,	1,	2,	3,	5,
    0.19,	0.0361,	0.006859,	0.00130321,	2,	1,	2,	3,
    100,	0,	1,	5,	3,	2,	1,	2,
    29,	30,	31,	0,	0,	0,	0,	1
  ))
  val vecbTest1 = new Matrix(8,1, List(
    0.3,
    41,
    60.7,
    77.68,
    86.242539,
    54.28798984,
    173,
    190
   ))


  val matA126 = new Matrix(3,2, List(
    1.0, 2.0,
    1.0, 5.0,
    0.0, 0.0
  ))
  val vecb126 = new Matrix(3, 1, List(4,0, 3.0, 9.0))

  val matA132 = new Matrix(4,2, List(
    1.0, 0.0,
    1.0, 1.0,
    1.0, 3.0,
    1.0, 4.0
    ))
  val vecb132 = new Matrix(4,1, List(0.0, 1.0, 2.0, 5.0))

  def test {
    println("matA20")
    println(matA20)
    println("vecb20")
    println(vecb20)
    println("bindMatrix")
    val bind = MathUtil.bindMatrix(matA20, vecb20)
    println(bind)
    println("forward")
    MathUtil.forward(bind)
    println(bind)
    println("x")
    println(MathUtil.backward(bind))
    println("gauss")
    println(MathUtil.gauss(matA20, vecb20))
    
    println("matATest1")
    println(matATest1)
    println("vecTest1")
    println(vecbTest1)
    println("gauss")
    println(MathUtil.gauss(matATest1, vecbTest1))

    println("transpose")
    println(matA58.transpose)
    
    println("least square")
    println("A132\n"+matA132)
    println("b132\n"+vecb132)
    val leastX = MathUtil.leastSquare(matA132, vecb132)
    println(leastX)
  }
}
