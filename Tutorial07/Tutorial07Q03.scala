object Tutorial07Q03 {
  def main(args: Array[String]): Unit = {
    val inputList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val outputList = filterPrime(inputList)
    println(outputList)
  }

  def filterPrime(inputList: List[Int]): List[Int] = {
    def isPrime(num: Int): Boolean = {
      if (num <= 1) false
      else if (num <= 3) true
      else {
        val sqrtNum = math.sqrt(num).toInt
        !(2 to sqrtNum).exists(num % _ == 0)
      }
    }

    inputList.filter(isPrime)
  }
}