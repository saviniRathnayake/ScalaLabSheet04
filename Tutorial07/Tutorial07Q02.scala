object Tutorial07Q02 {
  def main(args: Array[String]): Unit = {
    val inputList = List(1, 2, 3, 4, 5)
    val outputList = calculateSquare(inputList)
    println(outputList)
  }

  def calculateSquare (inputList: List[Int]): List[Int] = {
    inputList.map(number => number*number)
  }
}
