import scala.swing._

// set CLASSPATH=%CLASSPATH%;.\scala-library.jar;.\scala-swing.jar
// scalac SimpleTest.scala
// java SimpleTest

object SimpleTest extends SimpleSwingApplication
{
  def top = new MainFrame {
    title = "AHP test"
    object fieldNumOfCriteria extends TextField { columns = 5; text = "3" }
    object fieldNumOfAlternatives extends TextField { columns = 5; text = "4" }

    contents = new BoxPanel(Orientation.Vertical) {


      contents += new FlowPanel {
	contents += new Label("# of alternatives")
	contents += fieldNumOfAlternatives
	contents += new Label(",  # of criteria")
	contents += fieldNumOfCriteria
	contents += new Button("New") {
	  reactions += {
	    case event.ButtonClicked(source) => {
	      new NamesTable(
		try { fieldNumOfAlternatives.text.toInt } catch {case _  => 4 }
		,
		try {  fieldNumOfCriteria.text.toInt } catch {case _  => 3 }
		)
	    }
	  }
	}
      }

      contents += new Button("New") {
	reactions += {
	  //case event.ButtonClicked(source) => new AHPApplication(4,3)
	  case event.ButtonClicked(source) => new NamesTable(4,3)
	}
      }

      contents += new Button("Exit") {
	reactions += {
	  case event.ButtonClicked(source) => System.exit(0)
	  }
	}
      }


  }
}


class AHPApplication(val m:Int, val n:Int, val altNames:Seq[String], val cNames:Seq[String], val dAHP:DominantAHP) extends Frame
{
  title = "test"
  visible = true

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new Button("Close") {
      reactions += {
	case event.ButtonClicked(source) => close()
      }
    }

    contents += new TabbedPane {
      pages += new TabbedPane.Page("Dominant AHP", new DominantFrame(m,n,altNames,cNames, dAHP))
//      pages += new TabbedPane.Page("SPCM", new DominantFrame(m, n, altNames, cNames))
    }
  }  

}

class DominantFrame(val m:Int, val n:Int, val altNames:Seq[String], val cNames:Seq[String], val dAHP:DominantAHP) extends BoxPanel(Orientation.Vertical)
{
  def weightsView = new Label("weght")
  def matAView = new MatrixSheet(new Matrix(m,n), altNames, cNames){
    table.selection.elementMode = Table.ElementMode.Column
  }
  def aggregateView = new Label("score")

  contents += new Label("Criteria weights")
  contents += weightsView

  contents += new FlowPanel() {
    contents += new BoxPanel(Orientation.Vertical) {
      contents += new Label("Evaluation Matrix")
      contents += matAView
    }
    contents += new BoxPanel(Orientation.Vertical) {
      contents += new Label("Aggregate Scores")
      contents += aggregateView
    }
  }
}

class NamesTable(val m:Int, val n:Int) extends Frame
{
  title = "Alternatives and Criteria"
  visible = true

  val alternativesNames = (0 until m) map ("alt."+ _.toString)
  val criteriaNames = new Array[String](n)
  for(j <- 0 until n) criteriaNames(j) = new String("criteria" + j)

  val alternativesList = new Table() {
    override lazy val model = super.model.asInstanceOf[javax.swing.table.DefaultTableModel]
    model.addColumn("Alternatives")
    for(alt <- alternativesNames) model.addRow(Array[AnyRef](alt))
  }
  val criteriaList = new Table() {
    override lazy val model = super.model.asInstanceOf[javax.swing.table.DefaultTableModel]
    model.addColumn("Criteria")
    for(c <- criteriaNames) model.addRow(Array[AnyRef](c))
  }
				   
  contents = new BoxPanel(Orientation.Vertical){
    contents += new FlowPanel() {
      contents += new BoxPanel(Orientation.Vertical) {
	contents += new Label("Names of Alternatives")
	contents += alternativesList
      }
      contents += new BoxPanel(Orientation.Vertical) {
	yLayoutAlignment = 0.0
	contents += new Label("Names of Criteria")
	contents += criteriaList
      }
    }
    contents += new Button("New") {
      reactions += {
	case event.ButtonClicked(source) => {
	  val altNames = new Array[String](m)
	  for(i <- 0 until m) altNames(i) = new String(alternativesList.apply(i,0).toString)
	  val cNames = new Array[String](n)
	  for(j <- 0 until n) cNames(j) = new String(criteriaList.apply(j,0).toString)

	  val dAHP = new DominantAHP(m, n)
	  new AHPApplication(m,n,altNames,cNames, dAHP)
	  close()
	}
      }
    }
    
  }


}

class PCMFrame(val matrix:Matrix, val names:Seq[String], val dAHP:DominantAHP) extends Frame
{
  val m = matrix.m
  val n = matrix.n
  
  title = "PCM"
  visible = true

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel {
      contents += new Label("PCM")
      contents += new Label("eigenVec")
    }
    contents += new Button("OK") {
      reactions += {
	case event.ButtonClicked(source) => {
	  close()
	}
      }
    }
  }
}

class PCMSheet(val matrix:Matrix, val names:Seq[String]) extends ScrollPane
{
  val m = matrix.m
  val n = matrix.n

  val _tmpValues = Array.ofDim[Any](m,n)
  for(i <- 0 until m; j <- 0 until n) _tmpValues(i)(j) = matrix.at(i,j).toString
  val table = new Table(_tmpValues, names) {
    //selection.elementMode = Table.ElementMode.Column
    //rowHeight = m
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true
    gridColor = new java.awt.Color(100,100,100)

    override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
      new Label(matrix.at(i,j).toString) {
	listenTo(this)
	reactions += {
	  case event.MouseClicked(source, p, modif, clks,t) =>
            {
	      println(source)
	      println(p)
	      println(modif)
	      println(clks)
	      println(t)
	    }
	}
      }
    }

    listenTo(this.selection)
    reactions += {
      case event.TableColumnsSelected(source, range, adjust) =>
        {
	  println("hai col"+source.selection.columns.leadIndex)
	  //if(adjust) range.foreach(row => println("col: "+ row))
	  if(adjust) matrix.set(0, source.selection.columns.leadIndex,
				matrix.at(0,source.selection.columns.leadIndex)+1)
	}
    }
  }
  
  val rowHeader = new ListView(names) {
    fixedCellHeight = table.rowHeight
  }

  viewportView = table
  rowHeaderView = rowHeader
}


class MatrixSheet(val matrix:Matrix, val rowNames:Seq[String], val colNames:Seq[String]) extends ScrollPane
{
  val m = matrix.m
  val n = matrix.n

  val _tmpValues = Array.ofDim[Any](m,n)
  for(i <- 0 until m; j <- 0 until n) _tmpValues(i)(j) = matrix.at(i,j).toString

  val table = new Table(_tmpValues, colNames) {
    //selection.elementMode = Table.ElementMode.Column
    //rowHeight = m
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true
    gridColor = new java.awt.Color(100,100,100)

    override def rendererComponent(isSelected:Boolean, hasFocus:Boolean, i:Int, j:Int) = {
      new Label(matrix.at(i,j).toString) {
	reactions += {
	  case event.MouseClicked(source, p, modif, clks,t) =>
            {
	      println(source)
	      println(p)
	      println(modif)
	      println(clks)
	      println(t)
	    }
	}
      }
    }

    listenTo(this.selection)
    reactions += {
      case event.TableColumnsSelected(source, range, adjust) =>
        {
	  println("hai col"+source.selection.columns.leadIndex)
	  //if(adjust) range.foreach(row => println("col: "+ row))
	  if(adjust){
	    matrix.set(0, source.selection.columns.leadIndex,
		       matrix.at(0,source.selection.columns.leadIndex)+1)
	    new PCMFrame(dAHP.pcmOfA(source.selection.columns.leadIndex), rowNames,dAHP)
	  }
	}
    }
  }
  //for(i <- 0 until m; j <- 0 until n) table.update(i,j, matrix.at(i,j).toString)

  val rowHeader = new ListView(rowNames) {
    fixedCellHeight = table.rowHeight
  }

  viewportView = table
  rowHeaderView = rowHeader

}


