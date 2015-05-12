object Test {
  def main(args: Array[String]) = {
    //forDAHP
    //forDAHP3
    //forDAHP4
    forDAHP5
    //forMatrix
    //forSPCM
    //forSPCM2
    //forDAHPProp
  }

  def forDAHP4{
    val pcmC1 = new Matrix(3, 3, List(
      1.0, 1.0, 2.0,
      1.0, 1.0, 2.0,
      1.0/2, 1.0/2, 1.0
    ))
    val pcmC1p = new Matrix(3, 3, List(
      1.0, 3.0, 3.0,
      1.0/3, 1.0, 1.0,
      1.0/3, 1.0, 1.0
    ))
    val pcmC2p = new Matrix(3, 3, List(
      1.0, 1.0/3, 5.0,
      3.0, 1.0, 3.0,
      1.0/5, 1.0/3, 1.0
    ))
    val pcmC3p = new Matrix(3, 3, List(
      1.0, 1.0/5, (1.0/9.0),
      5.0, 1.0, 1.0/5,
      9.0, 5.0, 1.0
    ))
    val pcmC4p = new Matrix(3, 3, List(
      1.0, 1.0/5, 1.0/5,
      5.0, 1.0, 1.0/5,
      1.0/5, 5.0, 1.0
    ))
    val pcmA1 = new Matrix(4,4, List(
      1.0, 3.0, 3.0, 5.0,
      1.0/3, 1.0, 5.0, 8.0,
      1.0/3, 1.0/5, 1.0, 6.0,
      1.0/5, 1.0/8, 1.0/6, 1.0
    ))
    val pcmA1p = new Matrix(4,4, List(
      1.0, 3.0, 5.0, 9.0,
      1.0/3, 1.0, 5.0, 7.0,
      1.0/5, 1.0/5, 1.0, 5.0,
      1.0/9, 1.0/7, 1.0/5, 1.0
    ))
    val pcmA2 = new Matrix(4,4,List(
      1.0, 1.0/5, 3.0, 1.0/3,
      5.0, 1.0, 5.0, 3.0,
      1.0/3, 1.0/5, 1.0, 1.0/2,
      3.0, 1.0/3, 2.0, 1.0
    ))
    val pcmA2p = new Matrix(4,4,List(
      1.0, 1.0/5, 3.0, 1.0/3,
      5.0, 1.0, 5.0, 3.0,
      1.0/3, 1.0/5, 1.0, 1.0/3,
      3.0, 1.0/3, 3.0, 1.0
    ))
    val pcmA3 = new Matrix(4,4,List(
      1.0, 2.0, 1.0/4, 1.0/2,
      1.0/2, 1.0, 1.0/5, 1.0/6,
      4.0, 5.0, 1.0, 1.0,
      2.0, 6.0, 1.0, 1.0
    ))
    val pcmA3p = new Matrix(4,4,List(
      1.0, 3.0, 1.0/5, 1.0/3,
      1.0/3, 1.0, 1.0/5, 1.0/7,
      5.0, 5.0, 1.0, 1.0,
      3.0, 7.0, 1.0, 1.0
    ))
    val dahp4 = new DominantAHP(4,3)
    dahp4.setPCMofA(0, pcmA1p)
    dahp4.setPCMofA(1, pcmA2p)
    dahp4.setPCMofA(2, pcmA3p)
    dahp4.setPCMofB(0, pcmC1p)
    dahp4.setPCMofB(2, pcmC3p)
    dahp4.recalculateFromPCM
    println(dahp4)
    for(d <- dahp4.bs.keySet.toList.sorted) {
      println("支配代替案が "+dahp4.altNames(d) + " のときの総合評価値")
      println(dahp4.aggregateScore(d))
      for(alt <- 0 until dahp4.m) {
	println("規制代替案が " + dahp4.altNames(alt) + " のとき")
	println(dahp4.aggregateScore(d,alt))
      }
    }

    val ccm4 = dahp4.ccm
    println("CCMで調整後")
    println(ccm4)
    for(d <- dahp4.bs.keySet) {
      println("支配代替案が "+dahp4.altNames(d) + " のときの総合評価値")
      println(ccm4.aggregateScore(d))
      println("inner vector t")
      var tmpVec = dahp4.matA.rowDiagInverseMatrix(d)*ccm4.bs(d)
      tmpVec.normalizeColumn(0)
      println(tmpVec)
    }

    println("超一対比較行列: Harker法")
    val tmpSPCMHarker = dahp4.spcm
    tmpSPCMHarker.matHarker
    println(tmpSPCMHarker)
    val tmpPow = tmpSPCMHarker.powerMethod
    println(tmpPow._1)
    println(tmpPow._2)
    println("総合評価値")
    println(tmpSPCMHarker.aggregateScore(tmpPow._2))

  }

  def forDAHP5{
    val pcmC1 = new Matrix(3, 3, List(
      1.0, 1.0, 2.0,
      1.0, 1.0, 2.0,
      1.0/2, 1.0/2, 1.0
    ))
    val pcmC1p = new Matrix(3, 3, List(
      1.0, 3.0, 3.0,
      1.0/3, 1.0, 1.0,
      1.0/3, 1.0, 1.0
    ))
    val pcmC2p = new Matrix(3, 3, List(
      1.0, 1.0/3, 5.0,
      3.0, 1.0, 3.0,
      1.0/5, 1.0/3, 1.0
    ))
    val pcmC3p = new Matrix(3, 3, List(
      1.0, 1.0/5, (1.0/9.0),
      5.0, 1.0, 1.0/5,
      9.0, 5.0, 1.0
    ))
    val pcmC4p = new Matrix(3, 3, List(
      1.0, 1.0/5, 1.0/5,
      5.0, 1.0, 1.0/5,
      1.0/5, 5.0, 1.0
    ))
    val pcmA1 = new Matrix(4,4, List(
      1.0, 3.0, 3.0, 5.0,
      1.0/3, 1.0, 5.0, 8.0,
      1.0/3, 1.0/5, 1.0, 6.0,
      1.0/5, 1.0/8, 1.0/6, 1.0
    ))
    val pcmA1p = new Matrix(4,4, List(
      1.0, 3.0, 5.0, 9.0,
      1.0/3, 1.0, 5.0, 7.0,
      1.0/5, 1.0/5, 1.0, 5.0,
      1.0/9, 1.0/7, 1.0/5, 1.0
    ))
    val pcmA2 = new Matrix(4,4,List(
      1.0, 1.0/5, 3.0, 1.0/3,
      5.0, 1.0, 5.0, 3.0,
      1.0/3, 1.0/5, 1.0, 1.0/2,
      3.0, 1.0/3, 2.0, 1.0
    ))
    val pcmA2p = new Matrix(4,4,List(
      1.0, 1.0/5, 3.0, 1.0/3,
      5.0, 1.0, 5.0, 3.0,
      1.0/3, 1.0/5, 1.0, 1.0/3,
      3.0, 1.0/3, 3.0, 1.0
    ))
    val pcmA3 = new Matrix(4,4,List(
      1.0, 2.0, 1.0/4, 1.0/2,
      1.0/2, 1.0, 1.0/5, 1.0/6,
      4.0, 5.0, 1.0, 1.0,
      2.0, 6.0, 1.0, 1.0
    ))
    val pcmA3p = new Matrix(4,4,List(
      1.0, 3.0, 1.0/5, 1.0/3,
      1.0/3, 1.0, 1.0/5, 1.0/7,
      5.0, 5.0, 1.0, 1.0,
      3.0, 7.0, 1.0, 1.0
    ))
    val dahp = new DominantAHP(4,3)
    dahp.setPCMofA(0, pcmA1p)
    dahp.setPCMofA(1, pcmA2p)
    dahp.setPCMofA(2, pcmA3p)
    dahp.setPCMofB(0, pcmC1p)
    dahp.setPCMofB(2, pcmC3p)
    dahp.recalculateFromPCM
    println(dahp)

    //spcm
    val spcm = dahp.spcm
    println(spcm)

    // from spcm
    println("construct fron spcm")
    val dahpOut = new DominantAHP(dahp.m, dahp.n)
    dahpOut.setFromSPCM(spcm, dahp.bs.keySet)
    println(dahpOut)
    //DAHPUtil.writeExcelFile(dahpOut, "ttt.xahp")
    DAHPUtil.writeExcelFile(dahpOut, "ttt.xls")
    
    //read from excel
    println("read from excelFile")
    println(DAHPUtil.readExcelFile("ttt.xahp"))
    println(DAHPUtil.readExcelFile("ttt2.xahp"))
    println(DAHPUtil.readExcelFile("ttt2.xls"))

    //new AHPApplication(dahpOut)
  }

  def forSPCM{
    val dAHP100 = new DominantAHP(3,2, matA100, bs100)
    println(dAHP100)
    for(d <- bs100.keySet){
      println("dominant is" + d)
      println(dAHP100.aggregateScore(d))
    }

    println("SPCM")
    val matSPCM = dAHP100.spcm
    println(matSPCM)

    println("SPCM:Harker")
    matSPCM.matHarker
    println(matSPCM)

    println("SPCM:Harker solv")
    val tmpPow = matSPCM.powerMethod
    println(tmpPow._1)
    println(matSPCM.aggregateScore(tmpPow._2))

    println("SPCM test for Aggregate score")
    val dAHP2 = new DominantAHP(3,4)
    val spcm2 = dAHP2.spcm
    val vec2 = new Matrix(12,1, List(
0.0843
, 0.1596
, 0.032
, 0.1512
, 0.0169
, 0.0461
, 0.0408
, 0.0811
, 0.1666
, 0.153
, 0.0374
, 0.031
):List[Double])
    println(dAHP2)
    println(spcm2)
    println(vec2)
    println("score")
    println(spcm2.aggregateScore(vec2,0))

    // save and load
    println("test for save")
    println(dAHP100)
    println("save done")
  }

  def forDAHP {
    val dAHP100 = new DominantAHP(3,2, matA100, bs100)
    println(dAHP100)
    for(d <- bs100.keySet){
      println("dominant is" + d)
      println(dAHP100.aggregateScore(d))
    }

    printf("recalculate")
    dAHP100.recalculateFromPCM
    println(dAHP100)

    println("for CCM")
    val dAHP100_CCM = dAHP100.ccm
    println(dAHP100_CCM)
    for(d <- bs100.keySet){
      println("dominant is " + d)
      println(dAHP100_CCM.aggregateScore(d))
    }
  }

  def forDAHPProp {
    val dAHPProp = new DominantAHP(4,2, matAProp, bsProp)
    println(dAHPProp)
    for(d <- bsProp.keySet){
      println("dominant is" + d)
      println(dAHPProp.aggregateScore(d))
    }

    println("for CCM")
    val dAHPProp_CCM = dAHPProp.ccm
    println(dAHPProp_CCM)
    for(d <- bsProp.keySet){
      println("dominant is " + d)
      println(dAHPProp_CCM.aggregateScore(d))
    }
    println("for CCM at Pk")
    for(d <- bsProp.keySet){
      println("dominant is " + d)
      println(dAHPProp.matA.rowDiagInverseMatrix(d) * dAHPProp_CCM.bs(d))
    }
  }

  def forDAHP3 {
    val matC1A = new Matrix(3,3, List(
1.0, 1.0/3, 1.0/5,
3.0, 1.0, 1.0/3,
5.0, 3.0, 1.0))
    val matC2A = new Matrix(3,3, List(
1.0, 3.0, 7.0,
1.0/3, 1.0, 1.0,
1.0/7, 1.0, 1.0))
    val matA1C = new Matrix(2,2, List(
1.0, 5.0,
1.0/5, 1.0))
    val matA2C = new Matrix(2,2, List(
1.0, 3.0,
1.0/3, 1))
    val matA3C = new Matrix(2,2, List(
1.0, 1.0/3,
3.0, 1.0))
    val bs = Map( 0 -> new Matrix(2,1,1.0),
		  1 -> new Matrix(2,1,1.0),
		  2 -> new Matrix(2,1,1.0)
		)
    val dahp = new DominantAHP(3,2, new Matrix(3,2,1.0), bs)
    matC1A.copyTo(dahp.pcmOfA(0))
    matC2A.copyTo(dahp.pcmOfA(1))
    matA1C.copyTo(dahp.pcmOfBs(0))
    matA2C.copyTo(dahp.pcmOfBs(1))
    matA3C.copyTo(dahp.pcmOfBs(2))
    //dahp.pcmOfBs.put(0, matA1C)
    //dahp.pcmOfBs.put(1, matA2C)
    //dahp.pcmOfBs.put(2, matA3C)

    dahp.recalculateFromPCM
    println(dahp)
    println("評価行列A の第1行の要素を1にした場合")
    println(dahp.matA * dahp.matA.rowDiagInverseMatrix(0))

    for(d <- dahp.bs.keySet){
      println("支配代替案が " + dahp.altNames(d) + " のときの総合評価値(調整前)")
      println(dahp.aggregateScore(d))
    }

    println("CCMで調整後")
    val dahp_CCM = dahp.ccm
    println(dahp_CCM)
    for(d <- dahp.bs.keySet){
      println("支配代替案が " + dahp.altNames(d) + " のときの総合評価値(調整後)")
      println(dahp_CCM.aggregateScore(d))
    }


    println("超一対比較行列 SPCM")
    val matSPCM = dahp.spcm
    println(matSPCM)

    println("超一対比較行列 SPCM: Harker法を適用する場合")
    matSPCM.matHarker
    println(matSPCM)

    println("SPCM を Harker法で解いた場合")
    val tmpPow = matSPCM.powerMethod
    println("固有値: " + tmpPow._1)
    println("固有ベクトル(絶対的重要度の相対値):\n")
    println(tmpPow._2)
    println("総合評価値")
    //println(matSPCM.aggregateScore(tmpPow._2,0))
    println(matSPCM.aggregateScore(tmpPow._2))

  }

  val matA100 = new Matrix(3,2,  List(
1, 1,
3, 0.3,
5, 0.2
):List[Double])
  val bs100 = Map ( 0 ->  new Matrix(2,1, List(0.25, 0.75)),
		    1 ->  new Matrix(2,1, List(0.7, 0.3)),
		    2 ->  new Matrix(2,1, List(0.8, 0.2))
		  )

  val matAProp = new Matrix(4,2, List(
2.0, 1.0,
1.0/2.0, 2.0,
2.0/3.0, 3.0/4.0,
1.0, 2.0
))
  val bsProp = Map( 0 -> new Matrix(2,1, List(1.0/5.0, 4.0/5.0)),
		    1 -> new Matrix(2,1, List(3.0/5.0, 2.0/5.0)),
		    3 -> new Matrix(2,1, List(1.0/3.0, 2.0/3.0))
		    )

  def forSPCM2 {
    val bs = Map ( 0 -> new Matrix(4,1,1.0),
		   1 -> new Matrix(4,1,1.0),
		   2 -> new Matrix(4,1,1.0)
		   )
    val dahp = new DominantAHP(3,4, new Matrix(3,4,1.0), bs)
    matRAI.copyTo(dahp.pcmOfA(0))
    matRAII.copyTo(dahp.pcmOfA(1))
    matRAIII.copyTo(dahp.pcmOfA(2))
    matRAIV.copyTo(dahp.pcmOfA(3))
    
    dahp.pcmOfBs.put(0, matRC1)
    dahp.pcmOfBs.put(1, matRC2)
    dahp.pcmOfBs.put(2, matRC3)

    //println(dahp.pcmOfBs)
    dahp.recalculateFromPCM
    println(dahp)

    //println(dahp.matA * dahp.matA.rowDiagInverseMatrix(0))
    println("超一対比較行列 SPCM")
    val matSPCM = dahp.spcm
    val matSPCMlls = dahp.spcm
    println(matSPCM)

    println("超一対比較行列 SPCM: Harker法を適用する場合")
    matSPCM.matHarker
    println(matSPCM)

    println("SPCM を Harker法で解いた場合")
    val tmpPow = matSPCM.powerMethod
    println("固有値: " + tmpPow._1)
    println("固有ベクトル(絶対的重要度の相対値):\n")
    println(tmpPow._2)
    println("総合評価値")
    println(matSPCM.aggregateScore(tmpPow._2,0))

    val tmpPow2 = matSPCM.powerMethod2
    println("固有値: " + tmpPow2._1)
    println("固有ベクトル(絶対的重要度の相対値):\n")
    println(tmpPow2._2)
    println("総合評価値")
    println(matSPCM.aggregateScore(tmpPow2._2,0))
    //println(matSPCM * tmpPow2._2)


    dahp.recalculateFromPCM
    val mat_ccm = dahp.ccm
    //val mat_ccm = dahp.geomMeanCCM
    //println(dahp.aggregateScore(0,0))
    println("CCMで調整した場合")
    println(mat_ccm)
    println("CCMで調整した場合の総合評価値")
    println(mat_ccm.aggregateScore(0,0))

    println("lls")
    println(matSPCMlls.countNonZeroElements)
    val veclls = matSPCMlls.lls
    println(veclls)
    println("総合評価値")
    println(matSPCMlls.aggregateScore(veclls,0))

//    new AHPApplication(dahp)
  }

  val matRAI = new Matrix(3,3, List(
1.0, 1.0/3, 5.0,
3.0, 1.0, 3.0,
1.0/5, 1.0/3, 1.0))
  val matRAII = new Matrix(3,3, List(
1.0, 7.0, 3.0,
1.0/7, 1.0, 1.0/3,
1.0/3, 3.0, 1.0))
  val matRAIII = new Matrix(3,3, List(
1.0, 1.0/3, 1.0/3,
3.0, 1.0, 1.0/3,
3.0, 3.0, 1.0))
  val matRAIV = new Matrix(3,3, List(
1.0, 3.0, 5.0,
1.0/3, 1.0, 1.0,
1.0/5, 1.0, 1.0))
  val matRC1 = new Matrix(4,4, List(
1.0, 1.0/3, 3.0, 1.0/3,
3.0, 1.0, 3.0, 1.0,
1.0/3, 1.0/3, 1.0, 1.0/3,
3.0, 1.0, 3.0, 1.0))
  val matRC2 = new Matrix(4,4, List(
1.0, 9.0, 1.0, 5.0,
1.0/9, 1.0, 1.0/3, 1.0,
1.0, 3.0, 1.0, 1.0,
1.0/5, 1.0, 1.0, 1.0))
  val matRC3 = new Matrix(4,4, List(
1.0, 1.0/3, 1.0/9, 3.0,
3.0, 1.0, 1.0/5, 5.0,
9.0, 5.0, 1.0, 5.0,
1.0/3, 1.0/5, 1.0/5, 1.0))

  def forMatrix {
    val mat1 = new Matrix(2,3, List( ((0,0),0.1), ((1,1),0.2), ((1,2),0.3)))
    val mat2 = new Matrix(2,3, List( ((0,2),0.4), ((0,0),0.1), ((1,1),0.2), ((1,2),0.3)))

    println("mat1")
    println(mat1)
    println("mat2")
    println(mat2)

    println("add : mat1 + mat2")
    println(mat1 + mat2)
    println("sub : mat1 - mat2")
    println(mat1 - mat2)

    println("equals: mat1, mat1")
    println(mat1.equals(mat1))
    println("equals: mat1, mat2")
    println(mat1.equals(mat2))
    println("error: mat1, mat2")
    println(mat1.error(mat2))

    val mat3 = new Matrix(3,2, List(
1, 4,
2, 5,
3, 6
):List[Double])
    val mat4 = new Matrix(2,3, List(
7, 8, 9,
10, 11, 12
):List[Double])
    println("mat3")
    println(mat3)
    println("mat4")
    println(mat4)

    println("multiply : mat3 * mat4")
    println(mat3 * mat4)
    println("multiply : mat4 * mat3")
    println(mat4 * mat3)

    val matEmpty = new Matrix(3,3,1.0)
    println("mat: empty")
    println(matEmpty)
    println("setMultiply : mat3 * mat4 to matEmpty")
    mat3.setMultiply(mat4, matEmpty)
    println(matEmpty)



    val mat5 = new Matrix(5,5, List(
  1,   3,   3, 7, 9,
1.0/3,   1,   1, 5, 7,
1.0/3,   1,   1, 3, 5,
1.0/7, 1.0/5, 1.0/3, 1, 1,
1.0/9, 1.0/7, 1.0/5, 1, 1
):List[Double])
    println("mat5")
    println(mat5)
    println("powerMethod: mat5")
    val powerResult = mat5.powerMethod
    println("eigenValue = " + powerResult._1)
    println(powerResult._2)

    println("geomMeanMethod: mat5")
    val geomResult = mat5.geomMeanMethod
    println("eigenValue = " + geomResult._1)
    println(geomResult._2)
  

    println("diag")
    for(i <- 0 until mat5.m) println(i + ":\n" + mat5.rowDiagMatrix(i))

    println("diag inverse")
    for(i <- 0 until mat5.m) println(i + ":\n" + mat5.rowDiagInverseMatrix(i))

    println("mat1")
    println(mat1)
    println("mat5")
    println(mat5)
    println("setPartial: mat5.setPartial(mat1)")
    mat5.setPartial(1,4,mat1)
    println(mat5)
  }
}
