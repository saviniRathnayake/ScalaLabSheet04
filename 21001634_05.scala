import  scala.io.StdIn

object labsheet_05 {
  def main(args: Array[String]): Unit = {
    println("\n Question 01")
    println("Enter the number:")
    var input = StdIn.readInt()
    println(prime(input))

    println("\n Question 02")
    println("Enter the number:")
    var number=StdIn.readInt()
    var sequence= primeseq(number)
    println(sequence)

    println("\n Question 03")
    println("Enter the number:")
    var num=StdIn.readInt()
    var addition=sum(num)
    println(addition)

    println("\n Question 04")
    println("Enter the number:")
    var num2=StdIn.readInt()
    var even=isEven(num2)
    println(even)

    println("\n Question 05")
    println("Enter the number:")
    var num3=StdIn.readInt()
    var n=sumEven(num3)
    println(n)

    println("\n Question 06")
    println("Enter the number:")
    var num4=StdIn.readInt()
    var fib=fibonacciSeq(num4)
    println(fib)

  }

  //Question 01
  def prime(n:Int,i:Int=2):Boolean=n match{
    case n if(n==i)=>true
    case n if GCD(n,i)>1=>false
    case _=>prime(n,i+1)
  }
  def GCD(a:Int,b:Int):Int=b match{
    case b if b==0 =>a
    case b if b>a =>GCD(b,a)
    case _ => GCD(b,a%b)
  }
  //Question 02
  def primeseq(i:Int):Unit={
    if(i<2){
      return 0
    }
    primeseq(i-1)
    if (prime(i)){
      print(i + " ")
    }
  }

  //Question 03
  def sum(n:Int):Int ={
    if(n<=0)  0
    else n +sum(n-1)
  }

  //Question 04
  def isEven(n:Int):Boolean ={
    if(n==0) true
    else if(n==1) false
    else if(n<0) isEven(-n)
    else isEven(n-2)

  }

  //Question 05

  def sumEven(n:Int):Int={
    def helper(i:Int):Int={
      if(i<=0) 0
      else if(i%2==0) i+helper(i-2)
      else helper(i-1)
    }
    helper(n-2)
  }

  //Question 06

  def fibonacci(n:Int):Int=n match{
    case 0 =>0
    case x if x==1 => 1
    case _ => fibonacci(n-1)+fibonacci(n-2)
  }
  def fibonacciSeq(n:Int):Unit={
    if (n>0) fibonacciSeq(n-1)
    print(fibonacci(n ))
  }

}
