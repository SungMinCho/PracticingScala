
/// Chapter 1 ///

case class CreditCard(id : Int) {
  
}

case class Charge(cc : CreditCard, amount : Double) {
  def combine(other : Charge) : Charge = {
    if(cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Error")
  }
}

case class Coffee(id : Int, price : Double) {
  
}

class Cafe {
  def buyCoffee(cc : CreditCard) : (Coffee, Charge) = {
    val cup = new Coffee(0,5)
    (cup, Charge(cc, cup.price))
  }
  
  def buyCoffees(cc : CreditCard, n : Int) : (List[Coffee], Charge) = {
    val purchases : List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
  
  def coalesce(charges : List[Charge]) : List[Charge] = {
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
  }
}

/// Chapter 2 ///

/** Document Comment */
object MyModule {
  def abs(n : Int) : Int =
    if(n < 0) -n
    else n
  
  private def formatAbs(x : Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  
  def factorial(n : Int) : Int = {
    @annotation.tailrec
    def go(n : Int , acc : Int) : Int = {
      if(n <= 0) acc
      else go(n-1, acc*n)
    }
    go(n, 1)
  }
  
  def fibonacci(n : Int) : Int = {
    @annotation.tailrec
    def go(n : Int, a : Int, b : Int) : Int = {
      if(n <= 2) b
      else go(n-1, b, a+b)
    }
    if(n <= 1) 0
    else go(n,0,1)
  }
  
  def formatResultFor(name : String, from : Int, to : Int, f : Int => Int) = {
    for(i <- from to to)
      println(name + "(" + i + ") = " + f(i))
  }
  
  def findFirst[A](as : Array[A], p : A => Boolean) : Int = {
    @annotation.tailrec
    def loop(n : Int) : Int = {
      if(n >= as.length) -1
      else if(p(as(n))) n
      else loop(n+1)
    }
    
    loop(0)
  }
  
  def isSorted[A](as : Array[A], ordered : (A,A) => Boolean) : Boolean = {
    @annotation.tailrec
    def loop(n : Int) : Boolean = {
      if(n >= as.length) true
      else if(!ordered(as(n-1), as(n))) false
      else loop(n+1)
    }
    
    loop(1)
  }
  
  def curry[A,B,C](f : (A,B)=>C) : A => (B => C) = {
    a => (b => f(a,b))
  }
  
  def uncurry[A,B,C](f : A => B => C) : (A,B) => C = {
    (a,b) => f(a)(b)
  }
  
  def compose[A,B,C](f : B => C, g : A => B) : A => C = {
    a => f(g(a))
  }
  
  def main(args : Array[String]) = {
    println(formatAbs(-42))
    println("factorial(7) = " + factorial(7))
    formatResultFor("fibonacci", 1, 15, fibonacci)
    println(isSorted(Array(1,2,4,3), (a:Int,b:Int) => a < b))
  }
}

object MainApp{
  
  def main(args: Array[String]) : Unit = {
    println("Hello, world!")
    MyModule.main(args)
  }
}