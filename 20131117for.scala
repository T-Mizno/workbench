object Test {
  def time(process: => Unit) {
    val start = System.currentTimeMillis
    process
    println("Time:" + (System.currentTimeMillis - start) + "msec")
  }  

  def testForeach(n:Int) { (0 to n).foreach(i => {i+i; i*i; i-i; i^2})  }
  def testWhile(n:Int) {
    var i = 0
    while(i <= n) {
      {i+i; i*i; i-i; i^2}
      i = i + 1
    }
  }

  def test(n:Int) { time(testForeach(n)); time(testWhile(n))  }
}
