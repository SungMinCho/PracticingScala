
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

/// Chapter 4 ///

sealed trait MyOption[+A] {
  def map[B](f : A => B) : MyOption[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
  
  def getOrElse[B >: A](default: => B) : B = this match {
    case None => default
    case Some(v) => v
  }
  
  def flatMap[B](f : A => MyOption[B]) : MyOption[B] = this.map(f).getOrElse(None)
  
  def orElse[B >: A](ob: => MyOption[B]) : MyOption[B] = this.map(Some(_)).getOrElse(ob)
  
  def filter(f : A => Boolean) : MyOption[A] = this.flatMap((v) => if(f(v)) Some(v) else None)
  
  def isEmpty : Boolean = this match { case None => true case _ => false }
}

case object None extends MyOption[Nothing]
case class Some[+A](v : A) extends MyOption[A]

object Chapter4 {
  def mean(xs : Seq[Double]) : MyOption[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  
  def variance(xs : Seq[Double]) : MyOption[Double] = {
    val m : MyOption[Double] = mean(xs)
    def withm(m : Double) : MyOption[Double] = mean(xs.map((v) => math.pow(v-m,2)))
    m.flatMap(withm)
  }
  
  def lift[A,B](f : A => B) : MyOption[A] => MyOption[B] = _ map f
  val abs0 : MyOption[Double] => MyOption[Double] = lift(math.abs)
  
  def lift2[A,B,C](f : (A,B) => C) : (MyOption[A], MyOption[B]) => MyOption[C] = {
    (oa, ob) => {
      oa.map(MyModule.curry(f)).flatMap(ob.map(_))
    }
  }
  
  def Try[A](a: => A) : MyOption[A] =
    try Some(a)
    catch { case e : Exception => None }
  
  def map2[A,B,C](a : MyOption[A], b : MyOption[B])(f : (A,B) => C) : MyOption[C] = 
    lift2(f)(a,b)
  
  def sequence[A](a : MyList[MyOption[A]]) : MyOption[MyList[A]] = {
    def f(x : MyOption[A], y : MyOption[MyList[A]]) : MyOption[MyList[A]] =
      x.flatMap((v) => y.map((l) => Cons(v,l)))
    MyList.foldRight(a, Some(Nil):MyOption[MyList[A]])(f)
  }
  
  def traverse[A,B](a : MyList[A])(f : A => MyOption[B]) : MyOption[MyList[B]] = {
    def glue(x : A, y : MyOption[MyList[B]]) : MyOption[MyList[B]] =
      f(x).flatMap((v) => y.map(Cons(v,_)))
    MyList.foldRight(a, Some(Nil):MyOption[MyList[B]])(glue)
  }
  
  def sequence2[A](a : MyList[MyOption[A]]) : MyOption[MyList[A]] = traverse(a)((x)=>x)
}

sealed trait MyEither[+E, +A] {
  def map[B](f : A => B) : MyEither[E,B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }
  
  def flatMap[EE >: E, B](f : A => MyEither[EE,B]) : MyEither[EE,B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }
  
  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]) : MyEither[EE, B] = this match {
    case Left(_) => b
    case Right(v) => Right(v)
  }
  
  def map2[EE >: E, B, C](b : MyEither[EE,B])(f : (A,B) => C) : MyEither[EE, C] = {
    for {
      av <- this
      bv <- b
    } yield f(av,bv)
  }
}
case class Left[+E](v:E) extends MyEither[E, Nothing]
case class Right[+A](v:A) extends MyEither[Nothing, A]

object Chapter4Either {
  def Try[A](a: => A) : MyEither[Exception, A] =
    try Right(a)
    catch { case e : Exception => Left(e) }
  
  def traverse[E,A,B](as : MyList[A])(f : A => MyEither[E,B]) : MyEither[E, MyList[B]] = {
    def glue(a : A, l : MyEither[E, MyList[B]]) : MyEither[E, MyList[B]] =
      for {
        b <- f(a)
        bs <- l
      } yield Cons(b,bs)
    MyList.foldRight(as, Right(Nil):MyEither[E,MyList[B]])(glue)
  }
  
  def sequence[E,A](es : MyList[MyEither[E,A]]) : MyEither[E,MyList[A]] =
    traverse(es)((x) => x)
}

/// Chapter 5 ///

sealed trait MyStream[+A] {
  def headOption : MyOption[A] = this match {
    case Empty => None
    case Next(h,t) => Some(h())
  }
  
  def toList : List[A] = this match {
    case Empty => List()
    case Next(h,t) => h()::t().toList
  }
  
  def take(n : Int) : MyStream[A] = this match {
    case Empty => Empty
    case Next(h,t) => {
      if(n <= 0) Empty
      else Next(h, () => t().take(n-1))
    }
  }
  
  def drop(n : Int) : MyStream[A] = this match {
    case Empty => Empty
    case Next(h,t) => {
      if(n <= 0) this
      else t().drop(n-1)
    }
  }
  
  def takeWhile(p : A => Boolean) : MyStream[A] = this match {
    case Empty => Empty
    case Next(h,t) => {
      if(p(h())) Next(h, () => t().takeWhile(p))
      else Empty
    }
  }
  
  def exists(p : A => Boolean) : Boolean = this match {
    case Next(h, t) => p(h()) || t().exists(p)
    case _ => false
  }
  
  // allows more reusable recursion (f doesn't evaluate the second parameter (tail)) 
  def foldRight[B](z: => B)(f : (A, => B) => B) : B =
    this match {
    case Next(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  
  def forAll(p : A => Boolean) : Boolean = foldRight(true)((a,b) => p(a) && b)
  
  def exists2(p : A => Boolean) = foldRight(false)((a,b) => p(a) || b)
  def takeWhile2(p : A => Boolean) = 
    foldRight(Empty:MyStream[A])((a,b) => if(p(a)) Next(()=>a, ()=>b) else Empty)
  def headOption2 = foldRight(None:MyOption[A])((a,b) => Some(a))
  
  def map[B](f : A => B) : MyStream[B] =
    foldRight(Empty:MyStream[B])((a,b) => Next(()=>f(a), ()=>b))
  
  def filter(p : A => Boolean) : MyStream[A] =
    foldRight(Empty:MyStream[A])((a,b) => if(p(a)) Next(()=>a, ()=>b) else b)
  
  def flatMap[B](f : A => MyStream[B]) : MyStream[B] =
    foldRight(Empty:MyStream[B])(f(_) append _)
  
  def append[B >: A](other : MyStream[B]) : MyStream[B] =
    foldRight(other)(MyStream.next(_,_))
    
  def find(p : A => Boolean) : MyOption[A] =
    filter(p).headOption
   
  //unfold[A,S](z : S)(f : S => MyOption[(A,S)]) : MyStream[A]
  import MyStream.unfold
  def unfoldMap[B](f : A => B) : MyStream[B] =
    unfold(this){
      case Empty => None
      case Next(h,t) => Some((f(h()),t()))
    } // funny syntax seems like : (_ match {...}) <=> {...}
  
  def unfoldTake(n : Int) : MyStream[A] =
    unfold((this,n))((sn) => {
      val s = sn._1
      val n = sn._2
      if(n <= 0) None
      else s match {
        case Empty => None
        case Next(h,t) => Some((h(), (t(),n-1)))
      }
    })
  
  def unfoldTakeWhile(p : A => Boolean) : MyStream[A] =
    unfold(this){
      case Next(h,t) if(p(h())) => Some((h(), t())) // nice syntax : have if statement in case match
      case _ => None
    }
  
  def unfoldZipWith[B,C](other : MyStream[B])(f : (A,B) => C) : MyStream[C] =
    unfold((this,other)){
      case (Next(h1,t1), Next(h2,t2)) => Some((f(h1(),h2()), (t1(),t2())))
      case _ => None
    }
  
  def tail : MyStream[A] = this match {
    case Empty => Empty
    case Next(h,t) => t()
  }
  
  def unfoldZipAll[B](other : MyStream[B]) : MyStream[(MyOption[A], MyOption[B])] =
    unfold((this,other)){
      case (Empty, Empty) => None
      case (s1,s2) => Some( ( (s1.headOption, s2.headOption) , (s1.tail, s2.tail) ) )
    }
  
  def startsWith[B >: A](pre : MyStream[B]) : Boolean =
    this.unfoldZipAll(pre).unfoldTakeWhile(!_._2.isEmpty).forAll{ case (c1,c2) => c1 == c2 }
    // answer from github website
    // below is my first implementation :
    // this.unfoldZipWith(pre)(_ == _).forAll((x) => x)
    // -> wrong because of the case this.length < pre.length. my mistake
  
  def hasSubsequence[B >: A](pre : MyStream[B]) : Boolean =
    tails.exists(_ startsWith pre)
  
  def tails : MyStream[MyStream[A]] =
    unfold(this){
      case Next(h,t) => Some((Next(h,t), t()))
      case _ => None
    }.append(MyStream())
  
  def scanRight[B](z : B)(f : (A,B) => B) : MyStream[B] = this match {
    case Empty => MyStream(z)
    case Next(h,t) => {
      val res = t().scanRight(z)(f)
      res match {
        case Next(rh,_) => MyStream.next(f(h(),rh()),res)
        case _ => throw new Exception // never happens but dirty code...
      }
    }
  }  
  /*
  website implementation
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
  foldRight((z, Stream(z)))((a, p0) => {
    // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
    lazy val p1 = p0
    val b2 = f(a, p1._1)
    (b2, cons(b2, p1._2))
  })._2 
  */
}
case object Empty extends MyStream[Nothing]
case class Next[+A](h : () => A, t : () => MyStream[A]) extends MyStream[A]

object MyStream {
  def next[A](h: => A, t: => MyStream[A]) : MyStream[A] = {
    lazy val head = h
    lazy val tail = t
    Next(() => head, () => tail)
  }
  def empty[A] : MyStream[A] = Empty
  def apply[A](as : A*) : MyStream[A] =
    if(as.isEmpty) empty else next(as.head, apply(as.tail : _*))
  
  def constant[A](a : A) : MyStream[A] = next(a, constant(a))
  def from(n : Int) : MyStream[Int] = next(n, from(n+1))
  def fibsHelper(a : Int, b : Int) : MyStream[Int] = next(b, fibsHelper(b, a+b))
  def fibs() : MyStream[Int] = next(0, fibsHelper(0, 1))
  def unfold[A,S](z : S)(f : S => MyOption[(A,S)]) : MyStream[A] = f(z) match {
    case None => Empty
    case Some((a,s)) => next(a, unfold(s)(f))
  }
  
  val unfoldFibs = unfold((0,1))((s) => Some((s._1,(s._2,s._1+s._2))))
  def unfoldFrom(n : Int) = unfold(n)((n) => Some((n,n+1)))
  def unfoldConstant(n : Int) = unfold(n)((n) => Some((n,n)))
  val unfoldOnes = unfoldConstant(1)
}

/// Chapter 6 ///

trait RNG {
  def nextInt : (Int,RNG)
}

case class SimpleRNG(seed : Long) extends RNG {
  def nextInt : (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Chapter6 {
  def nonNegativeInt(rng : RNG) : (Int, RNG) = {
    val (i, next) = rng.nextInt
    if(i == Int.MinValue) (0,next)
    else if(i < 0) (-i,next)
    else (i,next)
  }
  
  def double(rng : RNG) : (Double, RNG) = {
    val (p, next) = nonNegativeInt(rng)
    if (p < Int.MaxValue) (p.toDouble / Int.MaxValue, next)
    else ((p-1).toDouble / Int.MaxValue, next)
  }
  
  def intDouble(rng : RNG) : ((Int,Double), RNG) = {
    val (i,r2) = rng.nextInt
    val (d,r3) = double(r2)
    ((i,d),r3)
  }
  
  def doubleInt(rng : RNG) : ((Double,Int), RNG) = {
    val ((i,d),r2) = intDouble(rng)
    ((d,i),r2)
  }
  
  def double3(rng : RNG) : ((Double,Double,Double), RNG) = {
    val (d1,r2) = double(rng)
    val (d2,r3) = double(r2)
    val (d3,r4) = double(r3)
    ((d1,d2,d3),r4)
  }
  
  def ints(count : Int)(rng : RNG) : (List[Int], RNG) = {
    if(count <= 0) (List(), rng)
    else {
      val (l, r) = ints(count-1)(rng)
      val (x, r2) = r.nextInt
      (x::l, r2)
    }
  }
  
  type Rand[+A] = RNG => (A, RNG)
  val int : Rand[Int] = _.nextInt
  def unit[A](a : A) : Rand[A] = rng => (a, rng)
  def map[A,B](s : Rand[A])(f : A => B) : Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  def nonNegativeEven : Rand[Int] = map(nonNegativeInt)(i => i - i%2)
  def double2 : Rand[Double] = 
    map(nonNegativeInt)((p) => (if (p < Int.MaxValue) p else (p-1)).toDouble / Int.MaxValue)
  
  def map2[A,B,C](ra : Rand[A], rb : Rand[B])(f : (A,B) => C) : Rand[C] =
    rng => {
      val(a, rng2) = ra(rng)
      val(b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  
  def both[A,B](ra : Rand[A], rb : Rand[B]) : Rand[(A,B)] = map2(ra,rb)((_,_))
  val randIntDouble2 : Rand[(Int,Double)] = both(int, double2)
  val randDoubleInt : Rand[(Double, Int)] = both(double2, int)
  
  def sequence[A](fs : List[Rand[A]]) : Rand[List[A]] =
    rng => {
      val (r,l) : (RNG, List[A]) =
        fs.foldRight((rng, List():List[A])){
          case (f, (r, l)) => {
            val (res, newr) = f(r)
            (newr, res::l)
          }
        }
        
      (l,r)
    }
  
  def ints2(count : Int) : Rand[List[Int]] = sequence(List.fill(count)(int))
  
  def flatMap[A,B](f : Rand[A])(g : A => Rand[B]) : Rand[B] = { rng =>
    val (a,rng2) = f(rng)
    g(a)(rng2)
  }
  
  def nonNegativeLessThan(n : Int) : Rand[Int] = 
    flatMap(nonNegativeInt)(p => r => {
      val mod = p % n
      if(p + (n-1) - mod >= 0) (mod, r)
      else nonNegativeLessThan(n)(r)
    })
  
  def mapByFlatMap[A,B](f : Rand[A])(g : A => B) : Rand[B] =
    flatMap(f)(a => r => (g(a), r))
  
  def map2ByFlatMap[A,B,C](ra : Rand[A], rb : Rand[B])(f : (A,B) => C) : Rand[C] =
    flatMap(ra)(a => rng2 => {
      val (b,rng3) = rb(rng2)
      (f(a,b), rng3)
    })
  
  def rollDie : Rand[Int] = map(nonNegativeLessThan(6))(_+1)
}

case class State[S,+A](run : S => (A,S)) {
  def map[B](f : A => B) : State[S,B] =
    State(s => { val (ax, as) = run(s); (f(ax), as) })
  
  def map2[B,C](sb : State[S,B])(f : (A,B) => C) : State[S,C] =
    State(
    s => { 
      val (sax, sas) = run(s)
      val(sbx, sbs) = sb.run(sas)
      (f(sax, sbx), sbs)
    })
  
  def flatMap[B](f : A => State[S,B]) : State[S,B] =
    State(s => { val (ax, as) = run(s); f(ax).run(as) })
}

object State {
  def unit[S,A](a : A) : State[S,A] = State(s => (a,s))
  
  // my bad solution
  def sequence_old[S,A](fs : List[State[S,A]]) : State[S,List[A]] =
    State(
    s => {
      fs.foldRight((List():List[A], s)){
        case (e, (rl, rs)) => {
          val (newe, news) = e.run(rs)
          (newe::rl, news)
        }
      }
    })
  
  // better version of foldRight.
  def sequence_right[S,A](fs : List[State[S,A]]) : State[S,List[A]] =
    fs.foldRight(unit[S,List[A]](List()))( (f:State[S,A], acc:State[S,List[A]]) =>
      acc.map2(f)((la,a) => a::la)
    )

  // official solution claims that foldLeft is actually faster than foldRight
  // reason being : The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequence[S,A](fs : List[State[S,A]]) : State[S,List[A]] =
    fs.reverse.foldLeft(unit[S,List[A]](List()))((acc,f)=>f.map2(acc)(_ :: _))
  
  def get[S] : State[S,S] = State(s => (s,s))
  def set[S](s:S) : State[S,Unit] = State(_ => ((), s))
  
  def modify[S](f : S => S) : State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked : Boolean, candies : Int, coins : Int)

object simMachine {
  // dirty solution. could use case match more elegantly.
  def work(m : Machine)(i : Input) : Machine = {
    val Machine(locked, candies, coins) = m
    if(candies <= 0) m
    else i match {
      case Coin => {
        if(locked) Machine(false, candies, coins+1)
        else m // do i still increase coin?
      }
      case Turn => {
        if(!locked) Machine(true, candies-1, coins)
        else m
      }
    }
  }
  
  def works(m : Machine)(li : List[Input]) : Machine = {
    li.foldLeft(m)(work(_)(_))
  }
  
  // my first solution
  def simulateMachine_old(inputs : List[Input]) : State[Machine, (Int,Int)] =
    State(
    m => {
      val newMachine = works(m)(inputs)
      val Machine(locked,candies,coins) = newMachine
      ((coins, candies), newMachine)
    })
  
  import State._
    
  // using sequence. it took some time to understand
  def simulateMachine(inputs : List[Input]) : State[Machine, (Int,Int)] = for {
    _ <- sequence(inputs.map(i => modify[Machine](m => work(m)(i))))
    s <- get
  } yield (s.coins, s.candies)
  // this is when i finally fully understood the for { ... <- ... } syntax.
  // everytime x <- y happens,
  // state s passes through and becomes a new state
  // and the result of that pass gets bound to x
  // 'get' is a way to bind the current state to the variable
  // 'set' is a way to set the current state to a desired state
  // (while producing meaningless unit value ())
  
  /*
   * looking at the official solution below,
   * think about how
   * (i => modify[Machine](m => work(m)(i))) : Input => State[Machine, Unit]
   *                                         =
   * (modify[Machine] _ compose work) : Input => State[Machine, Unit]
   * (actually my work : Machine => Input => Machine, while his work : Input => Machine => Machine
   * so if i suppose my work was : Input => Machine => Machine,
   * (i => modify[Machine](m => work(i)(m)))     =    (modify[Machine] _ compose work)
   * 
   * what is that _ there.
   * by experiment i found out that :
   * "Unapplied methods are only converted to functions when a function type is expected.
   * You can make this conversion explicit by writing `mult _` or `mult(_)` instead of `mult`."
   * so that's just
   * (i) => modify[Machine](work(i)) which makes sense and i understand it now.
   */
}
/*
 * The official solution
 object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}
 */

/// MainApp ///

object MainApp{
  
  def main(args: Array[String]) : Unit = {
    //println("Hello, world!")
    //MyModule.main(args)
    //println(MyList.product(MyList(1,2,3)))
    //println(MyList.drop(MyList(1,2,3,4,5,6), 3))
    //println(MyList.flatten(MyList(MyList(1,2,3),MyList(4,5,6),MyList(7,8,9))))
    /*val as = MyList(Some(1), Some(2), Some(3), Some(4))
    println(Chapter4.sequence(as))
    val bs = MyList(Some(1), Some(2), None, Some(4))
    println(Chapter4.sequence(bs))
    val cs = MyList(Right(1), Left("Error1"), Right(2), Left("Error2"), Right(3))
    println(Chapter4Either.sequence(cs))*/
    
    /*
    import MyStream._
    val s = next(1,next(2,next(3,empty)))
    println(s.drop(2).toList)
    println(s.headOption2)
    println(Empty.headOption2)
    println(s.map(_+4).toList)
    println(s.filter(_ % 2 == 0).toList)
    val bomb = next(1,next(2,next(3,next(sys.error("fail"), empty))))
    println(bomb)
    //println(bomb.toList)
    println(s.append(bomb))
    //// the book doesn't say lazy val, just val instead. that causes error
    //// which is : forward reference extends over definition of value ones
    ////lazy val ones : MyStream[Int] = next(1, ones)
    val ones = constant(1)
    println(ones.take(5).toList)
    println(ones.exists(_ % 2 != 0))
    println(fibs().take(15).toList)
    def tn(z:Int) = unfold(z)((s) => {val n = if (s%2==0) s/2 else 3*s+1; Some((n,n))})
    println(tn(1589).take(15).toList)
    println(unfoldFibs.take(15).toList)
    println(unfoldFrom(5).take(15).toList)
    println(ones.unfoldMap(_ + 2).take(15).toList)
    println(tn(1589).unfoldTake(15).toList)
    println(tn(1589).unfoldTakeWhile(_ > 150).toList)
    println(ones.unfoldZipWith(ones)(_+_).take(15).toList)
    println(ones.unfoldZipAll(s).take(15).toList)
    println(ones.startsWith(s))
    println(ones.startsWith(MyStream(1,1,1)))
    println(ones.startsWith(MyStream()))
    println(tn(1589).take(15).hasSubsequence(MyStream(596,298,149)))
    println(s.toList)
    println(s.scanRight(0)(_+_).toList)
    */
    
    /*
    import Chapter6._
    val r = SimpleRNG(1589)
    println(ints(15)(r))
    println(ints2(15)(r))
    println(double2(r))
    println(nonNegativeLessThan(100)(r))
    import simMachine._
    val inputs = List(Turn,Coin,Turn,Coin,Turn,Coin,Coin,Turn)
    println(simulateMachine(inputs).run(Machine(true,10,0)))
    */
    
  }
}