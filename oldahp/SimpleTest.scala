import scala.swing._
import java.io._
import javax.swing.filechooser.FileNameExtensionFilter

// set CLASSPATH=%CLASSPATH%;.;.\ext-libs\scala-library.jar;.\ext-libs\scala-swing.jar;.\ext-libs\poi-3.9-20121203.jar;.\ext-libs\commons-math3-3.2.jar
// scalac SimpleTest.scala
// java SimpleTest
//
//jar cvf test.jar *.class
//Main-Class: SimpleTest
//Class-Path: scala-library.jar scala-swing.jar
//jar cvfm test.jar META-INF\MANIFEST.MF *.class
//jar cvfm ahp.jar META-INF\MANIFEST.MF *.class org\ scala\
//

object Scales {
  val descriptions = Array(
    Array("選択肢", "一対評価値"),
    Array("きわめて劣っている", "1/9"),
    Array("非常に劣っている", "1/7"),
    Array("かなり劣っている", "1/5"),
    Array("やや劣っている", "1/3"),
    Array("同程度", "1"),
    Array("やや優れている", "3"),
    Array("かなり優れている", "5"),
    Array("非常に優れている", "7"),
    Array("きわめて優れている", "9")
  )
  val values = Array(1.0/9, 1.0/7, 1.0/5, 1.0/3, 1.0, 3.0, 5.0, 7.0, 9.0)
}

object SimpleTest extends SimpleSwingApplication
{
  def top = new MainFrame {
    try {
      val keyFile = new File("./key")
      if(! keyFile.exists()) {
	Dialog.showMessage(title="起動エラー", message="アプリケーションが起動できません。")
	System.exit(0)
      }
    } catch {
      case e:Exception => {
	Dialog.showMessage(title="起動エラー", message="アプリケーションが起動できません。")
	System.exit(0)
      }
    } finally{
      //System.exit(0)
    }


    title = "支配型AHP"
    object fieldNumOfCriteria extends TextField { columns = 5; text = "3" }
    object fieldNumOfAlternatives extends TextField { columns = 5; text = "4" }

    contents = new BoxPanel(Orientation.Vertical) {

      contents += new FlowPanel {
	contents += new Label("代替案の数＝")
	contents += fieldNumOfAlternatives
	contents += new Label(",  　評価基準の数＝")
	contents += fieldNumOfCriteria
	contents += new Button("作成") {
	  reactions += {
	    case event.ButtonClicked(source) => {
	      new NamesTable(
		try { fieldNumOfAlternatives.text.toInt } catch {case _  => 3 }
		,
		try {  fieldNumOfCriteria.text.toInt } catch {case _  => 2 }
		)
	    }
	  }
	}
      }

      contents += new FlowPanel(new Button("ファイルから読み込み") {
	reactions += {
	  case event.ButtonClicked(source) => {
	    val oDAHP = chooseAHPFromFile("ファイルから読み込み")
	    oDAHP match {
	      case Right(v) => new AHPApplication(v)
	      case Left(es:java.lang.IllegalStateException) => Dialog.showMessage(title="ファイル読み込みエラー", message="ファイルが壊れています。\n"+es)
	      case Left(e) => Dialog.showMessage(title="ファイル読み込みエラー", message="ファイルを読むことができません。\n"+e)
	    }
	  }
	}
      }
			      )

      //http://www.cis.upenn.edu/~matuszek/cis554-2011/Pages/scala-io-code-samples.html
      def chooseAHPFromFile(title: String = ""): Either[Exception, DominantAHP] = {  
	val chooser = new FileChooser(new File("."))
	chooser.title = title
	chooser.peer.setFileFilter(new FileNameExtensionFilter("ahpファイル", "*.xls","xls", "*.XLS","XLS"))
	chooser.showOpenDialog(this) match {
	  case FileChooser.Result.Approve => {
	    DAHPUtil.readExcelFile(chooser.selectedFile)
	  }
	  case _ => Left(new Exception("正しいファイルが選択されませんでした"))
	}
      }

      contents += new FlowPanel(new Button("終了") {
	reactions += {
	  case event.ButtonClicked(source) => System.exit(0)
	  }
	}
			      )
      }

  }
}

class NamesTable(val m:Int, val n:Int) extends Frame
{
  title = "代替案と評価基準"
  visible = true

  val alternativesNames = (0 until m).map(_+1)map("alt"+  _.toString)
  val alternativesNamesAndFlags = Array.ofDim[Any](m,2)
  for(i <- 0 until m) {
    alternativesNamesAndFlags(i)(0) = alternativesNames(i).toString
    alternativesNamesAndFlags(i)(1) = (Boolean box false)
  }
  alternativesNamesAndFlags(0)(1) = (Boolean box true)

  val criteriaNames = Array.ofDim[Any](n,1)
  for(j <- 0 until n) {
    criteriaNames(j)(0) = "criteria"+(j+1).toString
  }

  val alternativesList = new Table(alternativesNamesAndFlags, Array[Any]("名前","支配代替案?")) {
    rowHeight = 25
    //autoResizeMode = Table.AutoResizeMode.Off
  }

  val criteriaList = new Table(criteriaNames, Array[Any]("名前")){
    rowHeight = 25
    //autoResizeMode = Table.AutoResizeMode.Off
  }
				   
  contents = new BoxPanel(Orientation.Vertical){
    contents += new FlowPanel() {
      contents += new BoxPanel(Orientation.Vertical) {
	contents += new Label("代替案")
	contents += new ScrollPane(alternativesList) {
	  preferredSize = new Dimension(200,200)
	}
      }
      contents += new BoxPanel(Orientation.Vertical) {
	yLayoutAlignment = 0.0
	contents += new Label("評価基準")
	contents += new ScrollPane(criteriaList) {
	  preferredSize = new Dimension(100,200)
	}
      }
    }
    contents += new Button("作成") {
      reactions += {
	case event.ButtonClicked(source) => {
	  val altNames = new Array[String](m)
	  for(i <- 0 until m) altNames(i) = new String(alternativesList.apply(i,0).toString)
	  var bs:scala.collection.mutable.Map[Int, Matrix] = scala.collection.mutable.Map.empty
	  for(i <- 0 until m) {
	    if(alternativesList.apply(i,1).asInstanceOf[Boolean]) {
	      bs.put(i, (new Matrix(n, 1, 1.0)))
	      //bs += i ->(new Matrix(n, 1, i+0.1))
	    }
	  }
	  if(bs.size < 1)bs += 0 -> (new Matrix(n, 1, 1.0))
	  
	  val cNames = new Array[String](n)
	  for(j <- 0 until n) cNames(j) = new String(criteriaList.apply(j,0).toString)

	  val dAHP = new DominantAHP(m, n, (new Matrix(m,n,1.0)), bs.toMap)

	  dAHP.setAltNames(altNames)
	  dAHP.setCNames(cNames)
	  new AHPApplication(m,n,altNames,cNames, dAHP)
	  close()
	  //println(dAHP)
	  //altNames.map(println(_))
	  //bs.map(println(_))
	  //cNames.map(println(_))
	}
      }
    }
    
  }

  //size = new Dimension(500,500)
}


class AHPApplication(val m:Int, val n:Int, val altNames:Seq[String], val cNames:Seq[String], val aDAHP:DominantAHP) extends Frame
{
  title = "Main"
  visible = true

  def this(aaDAHP: DominantAHP) {
    this(aaDAHP.m, aaDAHP.n, aaDAHP.altNames, aaDAHP.cNames, aaDAHP)
  }


  contents = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel {
      contents += new Button("閉じる") {
	reactions += {
	  case event.ButtonClicked(source) => close()
	}
      }
      contents += new Label ("    ")
      contents += new Button("保存") {
	reactions += {
	  case event.ButtonClicked(source) => {
	    val oResult = chooseAHPFromFile("ファイルへの保存")
	    oResult match {
	      case Right(f) => Dialog.showMessage(title="ファイルへの保存", message="ファイル("+f+")へ保存しました。")
	      case Left(e) => Dialog.showMessage(title="ファイルへ書き込みエラー", message="ファイルへの保存に失敗しました。\n"+e)
	    }
	  }
	}
      }

      def chooseAHPFromFile(title: String = ""): Either[Exception, String] = {  
	val chooser = new FileChooser(new File("."))
	chooser.title = title
	chooser.peer.setFileFilter(new FileNameExtensionFilter("ahpファイル", "*.xls","xls", "*.XLS","XLS"))
	chooser.showSaveDialog(this) match {
	  case FileChooser.Result.Approve => {
	    var filename = chooser.selectedFile.toString
	    if(! filename.endsWith(".xls")) filename = filename + ".xls"
	    DAHPUtil.writeExcelFile(aDAHP, filename)
	  }
	  case _ => Left(new Exception("正しいファイルが選択されませんでした"))
	}
      }

    }
    contents += new FlowPanel {
      contents += new Button("CCM") {
	reactions += {
	  case event.ButtonClicked(source) => {
	    //new AHPApplication(m, n, altNames, cNames, dAHP.ccm)
	    new Frame {
	      title = "CCM"
	      visible = true
	      contents = new BoxPanel(Orientation.Vertical) {
		contents += new Button("閉じる") {
		  reactions += {
		    case event.ButtonClicked(source) => close
		  }
		}
		contents += new DominantFrame(aDAHP.ccm, false)
	      }
	    }
	  }
	}
      }
      contents += new Button("GMMDAHP") {
	reactions += {
	  case event.ButtonClicked(source) => {
	    //new AHPApplication(m, n, altNames, cNames, aDAHP.geomMeanCCM)
	    new Frame {
	      title = "GMMDAHP"
	      visible = true
	      contents = new BoxPanel(Orientation.Vertical) {
		contents += new Button("閉じる") {
		  reactions += {
		    case event.ButtonClicked(source) => close
		  }
		}
		contents += new DominantFrame(aDAHP.geomMeanCCM, false)
	      }
	    }
	  }
	}
      }
      contents += new Button("SPCM") {
	reactions += {
	  case event.ButtonClicked(source) => {
	    new SPCMFrame(aDAHP)
	  }
	}
      }//
    }
    contents += new TabbedPane {
      pages += new TabbedPane.Page("支配型AHP", new DominantFrame(aDAHP, true))
    }
  }

  class DominantFrame(val dAHP:DominantAHP, val isEditMode:Boolean) extends BoxPanel(Orientation.Vertical)
  {
    val score:Matrix = dAHP.aggregateScore(dAHP.bs.keySet.head)
    val dIds =   dAHP.bs.keySet.toList.sorted.toArray

    def weightsView = new WeightSheet
    def controlledWeightsView = new ControlledWeightSheet
    def matAView = new MatASheet
    def aggregateView = new ScoreSheet
    
    var currentDominant:Int = dIds(0)
    var currentControll:Int = 0

    dAHP.matA.normalizedByDiagInverse(currentControll)
    

    val aggregateScore:Matrix = dAHP.aggregateScore(currentDominant, currentControll)
    val normalizedAggregateScore:Matrix = dAHP.aggregateScore(currentDominant)
    val controlledWeights:Matrix = new Matrix(dAHP.n, 1)
    updateControlledWeights
    def updateControlledWeights {
      for(j <- 0 until dAHP.n) {
	controlledWeights.set(j,0, dAHP.matA.at(currentControll, j)/dAHP.matA.at(currentDominant,j) * dAHP.bs.apply(currentDominant).at(j,0))
      }
      controlledWeights.normalizeColumn(0)
    }
    
    val dominantLabel = new Label("支配代替案 : " + altNames(currentDominant)){
      foreground = new java.awt.Color(255,0,0)
    }
    val controllLabel = new Label("規制代替案 : " + altNames(currentControll)) {
      foreground = new java.awt.Color(0,0,255)
    }

    //preferredSize = new Dimension(800,500)

    def updateDominantFrame {
      //dAHP.recalculateFromPCM
      dAHP.matA.normalizedByDiagInverse(currentControll)
      dAHP.aggregateScore(currentDominant,currentControll).copyTo(aggregateScore)
      dAHP.aggregateScore(currentDominant).copyTo(normalizedAggregateScore)

      matAView.updateMatASheet
      weightsView.updateWeights
      controlledWeightsView.updateWeights
      aggregateView.scoresUpdate
      updateControlledWeights

      println("updateDominantFrame")
      dominantLabel.text = "支配代替案 : " + altNames(currentDominant)
      controllLabel.text = "規制代替案 : " + altNames(currentControll)

      println(dAHP)
      println("controlled weights" +controlledWeights)
      println(dAHP.aggregateScore(currentDominant))
      println(currentControll)
      println(currentDominant)
      println(aggregateScore)

      repaint()
    }


    contents += new FlowPanel() {
      contents += new BoxPanel(Orientation.Vertical) {
	contents += new Label("評価値行列")
	contents += matAView
      }
      contents += new BoxPanel(Orientation.Vertical) {
	contents += new Label("総合評価値")
	contents += aggregateView
      }
    }

    contents += new FlowPanel() {
      contents += new BoxPanel(Orientation.Vertical) {
	contents += new Label("支配代替案に対応する評価基準の重み")
	contents += weightsView
	contents += new Label("規制代替案にコントロールされた評価基準の重み")
	contents += controlledWeightsView
      }
      contents += new BoxPanel(Orientation.Vertical) {
	contents += dominantLabel
	contents += controllLabel
      }
    }

    class MatASheet extends ScrollPane
    {
      var currentFocusedColumn:Int = 0

      val _tmpValues = Array.ofDim[Any](m,n)
      for(i <- 0 until m; j <- 0 until n) _tmpValues(i)(j) = dAHP.matA.at(i,j).toString

      //preferredSize = new Dimension(400,200)
      preferredSize = new Dimension(400, Math.min(((new Table).peer.getRowHeight +1)* (m+1), 200))

      val table = new Table(_tmpValues, dAHP.cNames) {
	selection.elementMode = Table.ElementMode.Column
	//autoResizeMode = Table.AutoResizeMode.Off
	showGrid = true
	gridColor = new java.awt.Color(100,100,100)

	override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
	  new Label("%.3f".format(dAHP.matA.at(i,j))) {
	    if(i == currentControll) {
	      foreground = new java.awt.Color(0,0,255)
	    }
	    if( (j == currentFocusedColumn)  && isEditMode){
	      border = new javax.swing.border.LineBorder(new java.awt.Color(150,150,255), 1)
	    }
	    if(i == currentDominant) {
	      //foreground = new java.awt.Color(255,0,0)
	    }
	  }
	}
	override protected def editor(row:Int, column:Int) ={
	  if(column != currentFocusedColumn) {
	    currentFocusedColumn = column
	  }
	  else {
	    if(isEditMode) {
	      //println("showPCMFrame:"+column)
	      new PCMFrame(dAHP.pcmOfA(column), dAHP.matA, column, dAHP.altNames, "評価値行列Aの評価基準"+dAHP.cNames(column)+"について")
	    }
	  }

	  if(currentControll != row) {
	    if(! isEditMode) {currentControll = row}
	    updateDominantFrame
	  }
	  null
	}
      }

      val rowHeader = new ListView(dAHP.altNames) {
	fixedCellHeight = table.rowHeight
      }
      viewportView = table
      rowHeaderView = rowHeader

      def updateMatASheet {
	for(i <- 0 until m; j <- 0 until n) 
	{
	  table.update(i,j, dAHP.matA.at(i,j).toString)
	  table.updateCell(i,j)
	  //println(dAHP.matA.at(i,j).toString)
	}
	//println("update matA")
	//println(dAHP.matA)
      }
    }

    class WeightSheet extends ScrollPane
    {
      var currentFocusedRow = 0

      val _tmpValues = Array.ofDim[Any](dIds.size,n)
      for(i <- 0 until dIds.size; j <- 0 until n) {
	_tmpValues(i)(j) = dAHP.bs(dIds(i)).at(j,0)
      }
      val _tmpNames = new Array[String](dIds.size)
      for(i <- 0 until dIds.size) _tmpNames(i) = dAHP.altNames(dIds(i))

      //preferredSize = new java.awt.Dimension(400, 200);
      preferredSize = new java.awt.Dimension(400, Math.min(((new Table).peer.getRowHeight+1)* (dIds.size + 1), 200))

      def updateWeights {
	for(i <- 0 until dIds.size; j <- 0 until n) {
	  table.update(i,j, dAHP.bs(dIds(i)).at(j,0))
	  table.updateCell(i,j)
	}
      }

      val table = new Table(_tmpValues, dAHP.cNames) {
	//selection.elementMode = Table.ElementMode.Column
	//autoResizeMode = Table.AutoResizeMode.Off
	showGrid = true
	gridColor = new java.awt.Color(100,100,100)

	override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
	  new Label("%.3f".format(dAHP.bs(dIds(i)).at(j,0))) {
	    if(dIds(i) == currentDominant) {
	      foreground = new java.awt.Color(255, 0,0)
	      border = new javax.swing.border.LineBorder(new java.awt.Color(255,200,200), 1)
	    }
	  }
	}

	override protected def editor(row:Int, column:Int) ={
	  if(row != currentFocusedRow) {
	    currentFocusedRow = row
	    currentDominant = dIds(row)
	    updateDominantFrame
	  }
	  else {
	    if(isEditMode) {
	      new PCMFrame(dAHP.pcmOfBs(currentDominant), dAHP.bs(currentDominant), 0, dAHP.cNames, "支配代替案が"+dAHP.altNames(currentDominant)+"のときの評価基準の重みについて")
	    }	    
	  }
	  null
	}
      }

      val rowHeader = new ListView(_tmpNames){
	fixedCellHeight = table.rowHeight
	renderer = new ListView.Renderer[String]{
	  def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, a: String, index: Int): Component = {
	      new Label(dAHP.altNames(dIds(index))) {
		if(currentDominant == dIds(index)) {
		  foreground = new java.awt.Color(255, 0, 0)
		}
	      }
	  }
	}
      }

      viewportView = table
      rowHeaderView = rowHeader
    } // end of WeightSheet

    class ControlledWeightSheet extends ScrollPane
    {
      val _tmpValues = Array.ofDim[Any](1, n)
      for(j <- 0 until n) {
	_tmpValues(0)(j) = controlledWeights.at(j,0)
      }
      val _tmpNames = new Array[String](1)
      _tmpNames(0) = dAHP.altNames(currentControll)

      preferredSize = new java.awt.Dimension(400, Math.min(((new Table).peer.getRowHeight+1)* 2+1, 200))

      def updateWeights {
	for(j <- 0 until n) {
	  table.update(0,j, controlledWeights.at(j,0))
	  table.updateCell(0,j)
	}
      }

      val table = new Table(_tmpValues, dAHP.cNames) {
	//selection.elementMode = Table.ElementMode.Column
	//autoResizeMode = Table.AutoResizeMode.Off
	showGrid = true
	gridColor = new java.awt.Color(100,100,100)

	override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
	  new Label("%.3f".format(controlledWeights.at(j,0))) {
	      foreground = new java.awt.Color(0, 0, 255)
	  }
	}

	override protected def editor(row:Int, column:Int) ={
	  null
	}
      }

      val rowHeader = new ListView(_tmpNames){
	fixedCellHeight = table.rowHeight
	foreground = new java.awt.Color(0,0,255)
	renderer = new ListView.Renderer[String]{
	  def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, a: String, index: Int): Component = {
	    new Label(dAHP.altNames(currentControll)) {
	      foreground = new java.awt.Color(0,0,255)
	    }
	  }
	}

      }

      viewportView = table
      rowHeaderView = rowHeader
    } // end of ControlledWeightSheet

    class ScoreSheet extends ScrollPane
    {
      val _tmpValues = Array.ofDim[Any](m,2)
      for(i <- 0 until m) {
	_tmpValues(i)(0) = aggregateScore.at(i,0)
	_tmpValues(i)(1) = normalizedAggregateScore.at(i,0)
      }

      //preferredSize = new Dimension(150,200)
      //preferredSize = new Dimension(250,200)
      preferredSize = new Dimension(250, Math.min(((new Table).peer.getRowHeight+1)*(m+1), 200))

      val table = new Table(_tmpValues, Array("総合評価値", "正規化値")) {
	showGrid = true
	gridColor = new java.awt.Color(100,100,100)

	override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
	  var value = 0.0
	  if(j == 0) value = aggregateScore.at(i,0)
	  else if(j == 1) value = normalizedAggregateScore.at(i,0)

	  new Label("%.3f".format(value)) {
	    if( (i == currentControll) && (j == 0) ) {
	      foreground = new java.awt.Color(0,0,255)
	    }
	  }
	}

	override protected def editor(row:Int, column:Int) ={
	  if(currentControll != row) {
	    currentControll = row
	    updateDominantFrame
	  }
	  null
	}
      }
      
      def scoresUpdate() {
	for(i <- 0 until m) {
	  table.update(i,0, aggregateScore.at(i,0))
	  table.update(i,1, normalizedAggregateScore.at(i,0))
	  table.updateCell(i,0)
	  table.updateCell(i,1)
	}
      }

      val rowHeader = new ListView(dAHP.altNames){
	fixedCellHeight = table.rowHeight
      }
      viewportView = table
      rowHeaderView = rowHeader
    }




    class PCMFrame(val pcm:Matrix, val resultMatrix:Matrix, val resultCol:Int, val names:Seq[String], val aTitle:String) extends Frame
    {
      title = "一対比較行列 : " + aTitle
      visible = true

      contents = new BoxPanel(Orientation.Vertical) {
	contents += new Button("閉じる") {
	  reactions += {
	    case event.ButtonClicked(source) => {
	      close()
	    }
	  }
	}
	contents += new _PCMFrame(pcm, resultMatrix, resultCol, names, aTitle)
      }
    }

    class _PCMFrame(val pcm:Matrix, val resultMatrix:Matrix, val resultCol:Int, val names:Seq[String], val aTitle:String) extends BoxPanel(Orientation.Vertical)
    {
      val m = pcm.m
      val n = pcm.m
      var ci:Double = 0.0
      var currentCell:(Int, Int) = (0,0)
      
      var solvMethod:String = "power"

      val eigenVector = new Matrix(m, 1)
      for(i <- 0 until m) eigenVector.set(i,0, resultMatrix.at(i, resultCol))

      val pcmSheet = new PCMSheet
      val vectorSheet = new VectorSheet
      val ciLabel = new Label("%6.4f".format(ci))

      updatePCM


      val buttonList = List(
	new RadioButton("固有値法") { 
	  name = "power"
	  reactions += {
	    case event.ButtonClicked(buttonSelect) => {
	      solvMethod=name
	      updatePCM
	    }
	  }
	}
	,
	new RadioButton("幾何平均法") {
	  name = "geomMean"
	  reactions += {
	    case event.ButtonClicked(buttonSelect) => {
	      solvMethod=name
	      updatePCM
	    }
	  }
	}
      )
      def updateCIAndEigenVector {
	solvMethod match {
	  case "geomMean" => {
	    val tmpG = pcm.geomMeanMethod
	    tmpG._2.copyTo(eigenVector)
	    ci = funcCI(tmpG._1, n)
	  }
	  case _ => {
	    val tmpPow = pcm.powerMethod
	    tmpPow._2.copyTo(eigenVector)
	    ci = funcCI(tmpPow._1, n)
	  }
	}
      }

      def updatePCM {
	updateCIAndEigenVector

	pcmSheet.updatePCMSheet
	vectorSheet.updateEigenVector

	//println(solvMethod)
      }


      contents += new BoxPanel(Orientation.Vertical) {
	contents += new FlowPanel {
	  contents += pcmSheet
	  contents += vectorSheet
	}


	val buttonGroup= new ButtonGroup {
	  buttonList map { button =>
	    listenTo(button)
	    buttons.add(button)
	  }
	}
	buttonGroup.select(buttonList(0))


	contents += new FlowPanel {
	  contents += new BoxPanel(Orientation.Vertical) {
	    contents ++= buttonList
	  }
	  contents += new FlowPanel {
	    contents += new Label("CI= ")
	    contents += ciLabel
	  }

	  contents += new Button("適用") {
	    reactions += {
	      case event.ButtonClicked(source) =>
		//println("after apply fronm pcm")
		//println(resultMatrix)
		//println(resultCol)
		//println(eigenVector)
		resultMatrix.setPartial(0, resultCol, eigenVector)
		//println(resultMatrix)
		updateDominantFrame
	    }
	  }
	}
      }


      class PCMSheet extends ScrollPane
      {
	val _tmpValues = Array.ofDim[Any](m, m)
	for(i <- 0 until m; j <- 0 until m) _tmpValues(i)(j) = pcm.at(i,j).toString
	def updatePCMSheet {
	  for(ti <-0 until m; tj <- 0 until m) table.update(ti,tj,pcm.at(ti,tj).toString)
	}

	//preferredSize = new Dimension(500,300)
	preferredSize = new Dimension(500, Math.min( ((new Table).peer.getRowHeight + 1)*(m+1),300))

	val table = new Table(_tmpValues, names) {
	  selection.elementMode = Table.ElementMode.Cell
	  selection.intervalMode = Table.IntervalMode.Single
	  //rowHeight = m
	  //autoResizeMode = Table.AutoResizeMode.Off
	  showGrid = true
	  gridColor = new java.awt.Color(100,100,100)

	  override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
	    new Label("%.2f".format(pcm.at(i,j))){
	      if(pcm.near(pcm.at(i,j), 0.0)){
		foreground = new java.awt.Color(200,200,200)
	      }
	      if(i == j){
		foreground = new java.awt.Color(50,50,50)
	      }
	      if( (currentCell._1 == i) && (currentCell._2 == j) ){
		border = new javax.swing.border.LineBorder(new java.awt.Color(100,255,100), 2)
	      }

	    }
	  }

	  override protected def editor(i:Int, j:Int) ={
	    if(currentCell != (i,j)) {
	      currentCell = (i,j)
	    } else {
	      if( !pcm.near(pcm.at(i,j), 0.0) ) {
		currentCell = (i,j)
		//println(currentCell)
		if ( i != j) {
		  new Frame {
		    title = "スケール選択 : "+names(i)+"/"+names(j)
		    visible = true

		    preferredSize = new Dimension(300, 205)
		    
		    contents = new ScrollPane(new Table(Scales.descriptions.tail.toArray.map(_.toArray[Any]), Scales.descriptions.head) {

		      selection.elementMode = Table.ElementMode.Row
		      listenTo(this.selection)
		      reactions += {
			case event.TableRowsSelected(tmpS, tmpR, tmpA) => {
			  if(tmpA) {
			    val tmpVal = Scales.values(tmpS.selection.rows.leadIndex)
			    pcm.set(i,j,tmpVal)
			    pcm.set(j,i,1.0/tmpVal)
			    updatePCM
			    //println(tmpVal)
			    close()
			  }
			}
		      }
		    }
					    )
		  }
		}
	      }
	    }
	    null
	  }
	}


	val rowHeader = new ListView(names) {
	  fixedCellHeight = table.rowHeight
	}

	viewportView = table
	rowHeaderView = rowHeader
      }

      class VectorSheet extends BoxPanel(Orientation.Vertical)
      {
	val _tmpValues = Array.ofDim[Any](m, 1)
	for(i <- 0 until m) _tmpValues(i)(0) = eigenVector.at(i,0).toString

	val table = new Table(_tmpValues, Array("固有ベクトル")) {
	  //selection.elementMode = Table.ElementMode.Column
	  //rowHeight = m
	  //autoResizeMode = Table.AutoResizeMode.Off
	  showGrid = true
	  gridColor = new java.awt.Color(100,100,100)
	  
	  override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
	    new Label("%.4f".format(eigenVector.at(i,0)))
	  }

	}

	contents += new ScrollPane(table) {
	  val rowHeader = new ListView(names) {
	    fixedCellHeight = table.rowHeight
	  }
	  viewportView = table
	  rowHeaderView = rowHeader
	  //preferredSize = new Dimension(150,300)
	  preferredSize = new Dimension(150, Math.min( ((new Table).peer.getRowHeight + 1)*(m+1),300))
	}

	def updateEigenVector() {
	  for(i <- 0 until m) table.update(i,0, eigenVector.at(i,0).toString)
	  ciLabel.text = "%6.4f".format(ci)
	}
      }

      def funcCI(lambda:Double, aN:Int):Double = {
	//println("***** func ci ******")
	//println("lambda: " + lambda)
	//println("n: " + aN)
	(lambda-aN*1.0)/(aN*1.0-1.0)
      }

    }


  } // end of DominantFrame
}

class SPCMFrame(val dAHP:DominantAHP) extends Frame
{
  title = "SPCM"
  visible = true

  val spcm = dAHP.spcm
  spcm.matHarker

  val names = new Array[String](dAHP.m * dAHP.n)
  //for(i <- 0 until dAHP.m; j <- 0 until dAHP.n) names(i + j*dAHP.m) = "a"+(i+1)+":c"+(j+1)
  for(i <- 0 until dAHP.m; j <- 0 until dAHP.n) names(i + j*dAHP.m) = "c"+(j+1) + ":a"+(i+1)

  val vec = spcm.powerMethod._2

  var pivot = 0:Int
  val aggregateScore = spcm.aggregateScore(vec, pivot)
  val normalizedAggregateScore = aggregateScore.copy
  normalizedAggregateScore.normalizeColumn(0)

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new Button("閉じる") {
      reactions += {
	case event.ButtonClicked(source) => close
      }
    }
    
    contents += new FlowPanel {
      contents += new _SPCMFrame(spcm, vec, 0, names, "test")
      contents += new BoxPanel(Orientation.Vertical) {
	contents += new Label("総合評価値")
	contents += new SPCMScoreSheet 
	}
    }
  }
  
  def updateSPCMFrame {
    spcm.aggregateScore(vec, pivot).copyTo(aggregateScore)
    spcm.aggregateScore(vec).copyTo(normalizedAggregateScore)
    repaint()
  }

  class SPCMScoreSheet extends ScrollPane
  {
    val _tmpValues = Array.ofDim[Any](dAHP.m, 2)
    for(i <- 0 until dAHP.m) {
      _tmpValues(i)(0) = aggregateScore.at(i,0)
      _tmpValues(i)(1) = normalizedAggregateScore.at(i,0)
    }

    //preferredSize = new Dimension(250,200)
    preferredSize = new Dimension(250, Math.min( ((new Table).peer.getRowHeight +1) * (dAHP.m+1) , 200))

    val table = new Table(_tmpValues, Array("総合評価値", "正規化値")) {
      //selection.elementMode = Table.ElementMode.Column
      //autoResizeMode = Table.AutoResizeMode.Off
      showGrid = true
      gridColor = new java.awt.Color(100,100,100)

      override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
	if(j==0) {
	  new Label("%.3f".format(aggregateScore.at(i,0))){
	    if(i == pivot){
	      foreground = new java.awt.Color(0,0,255)
	    }
	  }
	} else {
	  new Label("%.3f".format(normalizedAggregateScore.at(i,0))){
	  }
	}
      }

      listenTo(this.selection)
      reactions += {
	case event.TableRowsSelected(source, range, adjust) =>
          {
	    pivot = source.selection.rows.leadIndex
	    //println("pivot "+pivot)
	    updateSPCMFrame
	  }
      }
    }

    val rowHeader = new ListView(dAHP.altNames){
      fixedCellHeight = table.rowHeight
    }
    viewportView = table
    rowHeaderView = rowHeader
  }
//}

  class _SPCMFrame(val pcm:SPCM, val resultMatrix:Matrix, val resultCol:Int, val names:Seq[String], val aTitle:String) extends BoxPanel(Orientation.Vertical)
  {
    val m = pcm.m
    val n = pcm.m
    var ci:Double = 0.0
    var currentCell:(Int, Int) = (0,0)
    
    var solvMethod:String = "power"

    val eigenVector = new Matrix(m, 1)
    for(i <- 0 until m) eigenVector.set(i,0, resultMatrix.at(i, resultCol))

    val pcmSheet = new PCMSheet
    val vectorSheet = new VectorSheet
    val ciLabel = new Label("%6.4f".format(ci))

    updatePCM

    val buttonList = List(
      new RadioButton("固有値法(Harker法)") { 
	name = "power"
	reactions += {
	  case event.ButtonClicked(buttonSelect) => {
	    solvMethod=name
	    updatePCM
	  }
	}
      }
      ,
      new RadioButton("対数最小二乗法") {
	name = "lls"
	reactions += {
	  case event.ButtonClicked(buttonSelect) => {
	    solvMethod=name
	    updatePCM
	  }
	}
      }
    )
    def setOneDiag{
      for(i <- 0 until m; j <- 0 until n) if(i==j) pcm.set(i,j, 1.0)
    }
    def updateCIAndEigenVector {
      solvMethod match {
	case "lls" => {
	  setOneDiag
	  val tmpV = pcm.lls
	  tmpV.normalizeColumn(0)
	  tmpV.copyTo(eigenVector)
	  ci = 0.0
	}
	case _ => {
	  pcm.matHarker
	  val tmpPow = pcm.powerMethod
	  tmpPow._2.copyTo(eigenVector)
	  ci = funcCI(tmpPow._1, n)
	}
      }
    }

    def updatePCM {
      updateCIAndEigenVector

      pcmSheet.updatePCMSheet
      vectorSheet.updateEigenVector

      resultMatrix.setPartial(0, resultCol, eigenVector)

      updateSPCMFrame
    }


    contents += new BoxPanel(Orientation.Vertical) {
      contents += new FlowPanel {
	contents += new BoxPanel(Orientation.Vertical) {
	  contents += new Label("超一対比較行列")
	  contents += pcmSheet
	}
	contents += new BoxPanel(Orientation.Vertical) {
	  contents += new Label("絶対的重要度")
	  contents += vectorSheet
	}
      }	       

      val buttonGroup= new ButtonGroup {
	buttonList map { button =>
	  listenTo(button)
	  buttons.add(button)
	}
      }
      buttonGroup.select(buttonList(0))


      contents += new FlowPanel {
	contents += new BoxPanel(Orientation.Vertical) {
	  contents ++= buttonList
	}
	contents += new FlowPanel {
	  contents += new Label("CI= ")
	  contents += ciLabel
	}

	contents += new Button("適用") {
	  reactions += {
	    case event.ButtonClicked(source) =>
	      //println("after apply fronm pcm")
	      //println(resultMatrix)
	      //println(resultCol)
	      //println(eigenVector)
	      resultMatrix.setPartial(0, resultCol, eigenVector)
	      dAHP.setFromSPCM(spcm, dAHP.bs.keySet)
	      //println(resultMatrix)
	  }
	}
      }
    }


    class PCMSheet extends ScrollPane
    {
      val _tmpValues = Array.ofDim[Any](m, m)
      for(i <- 0 until m; j <- 0 until m) _tmpValues(i)(j) = pcm.at(i,j).toString
      def updatePCMSheet {
	for(ti <-0 until m; tj <- 0 until m) table.update(ti,tj,pcm.at(ti,tj).toString)
      }

      //preferredSize = new Dimension(500,300)
      preferredSize = new Dimension(500, Math.min( ( (new Table).peer.getRowHeight +2 )*(m+1)-2, 300))

      val table = new Table(_tmpValues, names) {
	selection.elementMode = Table.ElementMode.Cell
	selection.intervalMode = Table.IntervalMode.Single
	//rowHeight = m
	//autoResizeMode = Table.AutoResizeMode.Off
	showGrid = true
	gridColor = new java.awt.Color(100,100,100)

	override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
	  new Label("%.2f".format(pcm.at(i,j))){
	    if(pcm.near(pcm.at(i,j), 0.0)){
	      foreground = new java.awt.Color(200,200,200)
	    }
	    if((i == j) && (solvMethod == "power") ){
	      foreground = new java.awt.Color(255,100,255)
	    }
	    if( (currentCell._1 == i) && (currentCell._2 == j) ){
	      border = new javax.swing.border.LineBorder(new java.awt.Color(100,255,100), 2)
	    }else{
	      border = new javax.swing.border.LineBorder(new java.awt.Color(255,255,255), 1)
	    }

	  }
	}

	override protected def editor(i:Int, j:Int) ={
	  if(currentCell != (i,j)) {
	    currentCell = (i,j)
	  } else {
	    if( !pcm.near(pcm.at(i,j), 0.0) ) {
	      currentCell = (i,j)
	      //println(currentCell)
	      if ( i != j) {
		new Frame {
		  title = "スケール選択 : "+names(i)+"/"+names(j)
		  visible = true

		  preferredSize = new Dimension(300, 205)

		  contents = new ScrollPane(new Table(Scales.descriptions.tail.toArray.map(_.toArray[Any]), Scales.descriptions.head) {
		    selection.elementMode = Table.ElementMode.Row

		    listenTo(this.selection)
		    reactions += {
		      case event.TableRowsSelected(tmpS, tmpR, tmpA) => {
			if(tmpA) {
			  val tmpVal = Scales.values(tmpS.selection.rows.leadIndex)
			  pcm.set(i,j,tmpVal)
			  pcm.set(j,i,1.0/tmpVal)
			  updatePCM
			  //println(tmpVal)
			  close()
			}
		      }
		    }
		  }
					  )
		}
	      }
	    }
	  }
	  null
	}
      }


      val rowHeader = new ListView(names) {
	fixedCellHeight = table.rowHeight
      }

      viewportView = table
      rowHeaderView = rowHeader
    }

    class VectorSheet extends BoxPanel(Orientation.Vertical)
    {
      val _tmpValues = Array.ofDim[Any](m, 1)
      for(i <- 0 until m) _tmpValues(i)(0) = eigenVector.at(i,0).toString

      val table = new Table(_tmpValues, Array("正規化値")) {
	//selection.elementMode = Table.ElementMode.Column
	//rowHeight = m
	//autoResizeMode = Table.AutoResizeMode.Off
	showGrid = true
	gridColor = new java.awt.Color(100,100,100)
	
	override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
	  new Label("%.4f".format(eigenVector.at(i,0)))
	}

      }

      contents += new ScrollPane(table) {
	val rowHeader = new ListView(names) {
	  fixedCellHeight = table.rowHeight
	}
	viewportView = table
	rowHeaderView = rowHeader
	//preferredSize = new Dimension(150,300)
	preferredSize = new Dimension(150, Math.min( ( (new Table).peer.getRowHeight +2 )*(m+1)-2, 300))
      }

      def updateEigenVector() {
	for(i <- 0 until m) table.update(i,0, eigenVector.at(i,0).toString)
	ciLabel.text = "%6.4f".format(ci)
	if(solvMethod == "lls") {
	  ciLabel.text = " ******"
	}
      }
    }

    def funcCI(lambda:Double, aN:Int):Double = {
      //println("***** func ci ******")
      //println("lambda: " + lambda)
      //println("n: " + aN)
      (lambda-aN*1.0)/(aN*1.0-1.0)
    }

  }
}
