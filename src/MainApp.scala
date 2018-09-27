
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

/// Chapter 3 ///

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head : A, tail : MyList[A]) extends MyList[A]

object MyList{
  def sum(ints : MyList[Int]) : Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  
  def product(ds : MyList[Double]) : Double = ds match {
    case Nil => 1
    case Cons(0,_) => 0
    case Cons(x,xs) => x * product(xs)
  }
  
  def tail[A](xs : MyList[A]) : MyList[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => t
  }
  
  def setHead[A](xs : MyList[A], h : A) : MyList[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }
  
  def drop[A](l : MyList[A], n : Int) : MyList[A] = {
    if(n<=0) l
    else
      l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
      }
  }
  
  def dropWhile[A](l : MyList[A], f : A => Boolean) : MyList[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if(f(h)) dropWhile(t, f)
      else l
    }
  }
  
  def dropWhileCurry[A](l : MyList[A])(f : A => Boolean) : MyList[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if(f(h)) dropWhileCurry(t)(f)
      else l
    }
  }
  
  def foldRight[A, B](l : MyList[A], z : B)(f : (A,B) => B) : B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }
  
  def sum2(ns : MyList[Int]) = foldRight(ns, 0)(_ + _)
  def product2(ns : MyList[Double]) = foldRight(ns, 1.0)(_ * _)
  def length2[A](l : MyList[A]) : Int = foldRight(l, 0)((_,len) => len+1)
  
  def foldLeft[A,B](l : MyList[A], z : B)(f : (B, A) => B) : B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z, h))(f)
  }
  
  def sum3(ns : MyList[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns : MyList[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length3[A](l : MyList[A]) : Int = foldLeft(l, 0)((len,_) => len+1)
  
  def reverse[A](l : MyList[A]) : MyList[A] = foldLeft(l, Nil:MyList[A])((z, h) => Cons(h,z))
  
  def append[A](a1 : MyList[A], a2 : MyList[A]) : MyList[A] = {
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  }
  
  def append2[A](a1 : MyList[A], a2 : MyList[A]) = foldRight(a1, a2)(Cons(_,_))
  
  def foldLeftByRight[A,B](l : MyList[A], z : B)(f : (B, A) => B) : B =
    foldRight(l, (b:B)=>b)((h,g) => (z) => g(f(z,h)))(z)
  
  def flatten[A](xss : MyList[MyList[A]]) : MyList[A] =
    foldRight(xss, Nil:MyList[A])((a,b) => append(a,b))
  
  def init[A](l : MyList[A]) : MyList[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
  
  def map[A,B](l : MyList[A])(f : A => B) : MyList[B] = l match {
    case Nil => Nil
    case Cons(h,t) => Cons(f(h), map(t)(f))
  }
  
  def filter[A](l : MyList[A])(f : A => Boolean) : MyList[A] = l match {
    case Nil => Nil
    case Cons(h,t) => {
      if(f(h)) Cons(h, filter(t)(f))
      else filter(t)(f)
    }
  }
  
  def flatMap[A,B](l : MyList[A])(f : A => MyList[B]) : MyList[B] = flatten(map(l)(f))
  
  def filterByFlatMap[A](l : MyList[A])(f : A => Boolean) : MyList[A] =
    flatMap(l)((e) => if(f(e)) MyList(e) else Nil)
  
  def zipWith[A,B,C](l1 : MyList[A], l2 : MyList[B])(f : (A,B) => C) : MyList[C] = l1 match {
    case Nil => Nil
    case Cons(h,t) => {
      l2 match {
        case Nil => Nil
        case Cons(h2, t2) => Cons(f(h,h2), zipWith(t,t2)(f)) 
      }
    }
  }
  
  def take[A](l : MyList[A], n : Int) : MyList[A] = l match {
    case Nil => Nil
    case Cons(h,t) => {
      if(n <= 0) Nil
      else Cons(h, take(t, n-1))
    }
  }
  
  def length[A](x:MyList[A]) = length2(x)
  
  //unefficient
  def hasSubsequence[A](sup : MyList[A], sub : MyList[A]) : Boolean = {
    if(length(sub) == 0) true
    else if(length(sup) < length(sub)) false
    else if(take(sup, length(sub)) == sub) true
    else hasSubsequence(tail(sup), sub)
  }
  
  def apply[A](as : A*) : MyList[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail  : _*))
  }
}

sealed trait MyTree[+A]
case class Leaf[A](v : A) extends MyTree[A]
case class Branch[A](left : MyTree[A], right : MyTree[A]) extends MyTree[A]

object MyTree{
  def size[A](t : MyTree[A]) : Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }
  
  def maximum(t : MyTree[Int]) : Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }
  
  def depth[A](t : MyTree[A]) : Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
  
  def map[A,B](t : MyTree[A])(f : A => B) : MyTree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
  
  def fold[A,B](t : MyTree[A])(z : A => B)(f : (B,B) => B) : B = t match {
    case Leaf(v) => z(v)
    case Branch(l,r) => f(fold(l)(z)(f), fold(r)(z)(f)) 
  }
  
  def size2[A](t : MyTree[A]) = fold(t)(_ => 1)(1+_+_)
  def maximum2(t : MyTree[Int]) = fold(t)((a)=>a)((m1,m2) => m1 max m2)
  def depth2[A](t : MyTree[A]) = fold(t)(_ => 0)((m1,m2) => 1 + (m1 max m2))
  def map2[A,B](t : MyTree[A])(f : A => B) : MyTree[B] = 
    fold(t)((v:A) => Leaf(f(v)):MyTree[B])(Branch(_,_))
}

object MainApp{
  
  def main(args: Array[String]) : Unit = {
    println("Hello, world!")
    //MyModule.main(args)
    println(MyList.product(MyList(1,2,3)))
    println(MyList.drop(MyList(1,2,3,4,5,6), 3))
    println(MyList.flatten(MyList(MyList(1,2,3),MyList(4,5,6),MyList(7,8,9))))
  }
}