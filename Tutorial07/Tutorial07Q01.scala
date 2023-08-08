object Tutorial07Q01 {

    def main(args: Array[String]): Unit = {
      val inputList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val outputList = filterEvenNumbers(inputList)
      println(outputList)
    }
    def filterEvenNumbers(inputList: List[Int]): List[Int] = {
      inputList.filter(number => number % 2 == 0)
    }

}
