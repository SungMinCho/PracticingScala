import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]
  def run[A](s : ExecutorService)(a : Par[A]) : Future[A] = a(s)  
  
  private case class UnitFuture[A](get : A) extends Future[A] {
    def isDone = true
    def get(timeout : Long, units : TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning : Boolean) : Boolean = false
  }
  
  def unit[A](a : A) : Par[A] = es => UnitFuture(a)
  
  // fork is the only way to create new thread for the Par computation
  def fork[A](pa: => Par[A]) : Par[A] = 
    es => es.submit(new Callable[A] {
      def call = pa(es).get
    })
  
  // not a primitive
  def lazyUnit[A](a : A) : Par[A] = fork(unit(a))
  
  // question : would it be parallel? i don't think so because f will be strict on its arguments
  //def map2[A,B,C](pa : Par[A], pb : Par[B])(f : (A,B) => C) : Par[C] =
  //  es => UnitFuture(f(pa(es).get, pb(es).get))
  // so i think that's why the book does it this way
  def map2[A,B,C](pa : Par[A], pb : Par[B])(f : (A,B) => C) : Par[C] =
    es => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get,bf.get))
    }
  // yeah because each thread will start running on the creation of the Futures af and bf
  // thus becoming parallel
  
  def asyncF[A,B](f : A => B) : A => Par[B] =
    a => lazyUnit(f(a))
  
  def map[A,B](pa : Par[A])(f : A => B) : Par[B] =
    map2(pa, unit(()))((a,_) => f(a))
  
  def sequence[A](ps : List[Par[A]]) : Par[List[A]] =
    ps.foldRight(unit(List():List[A]))((pa, pas) => map2(pa,pas)(_::_))
  // the official solution suggests a more efficient sequence using split and append
  
  // my first solution : 
  //def parMap[A,B](ps : List[A])(f : A => B) : Par[List[B]] =
  //  sequence(ps.map(asyncF(f)))
  // the book :
    def parMap[A,B](ps : List[A])(f : A => B) : Par[List[B]] = fork {
      val fbs = ps.map(asyncF(f))
      sequence(fbs)
    }
  // difference 1 : fork (returns immediately...)
  // difference 2 : two lines instead of one. (important? this time i don't really think so)
  
  def flatSequence[A](ps : List[Par[Option[A]]]) : Par[List[A]] =
    ps.foldRight(unit(List():List[A]))((pa, pas) => 
      map2(pa,pas)((a,as) => a.map(_::as).getOrElse(as)))
      
  def generalSequence[A,B](ps : List[Par[A]])(f : (A,B) => B)(z : Par[B]) : Par[B] =
    ps.foldRight(z)((pa,pas) => map2(pa,pas)(f))
  // can make flatSequence and sequence out of this
  
  // different from book but i think this is faster.
  def parFilter[A](as : List[A])(f : A => Boolean) : Par[List[A]] = fork {
    val fo : A => Option[A] = (a) => if(f(a)) Option(a) else Option.empty
    val foas : List[Par[Option[A]]] = as.map(asyncF(fo))
    flatSequence(foas)
  }
  
  def recurse[A,B](as : IndexedSeq[A])(combine : (B,B) => B)(base : Option[A] => B) : Par[B] =
    if(as.length <= 1) unit(base(as.headOption))
    else {
      val (l,r) = as.splitAt(as.length / 2)
      map2(fork(recurse(l)(combine)(base)), 
           fork(recurse(r)(combine)(base)))(combine)
    }
  
  def maximum(as : IndexedSeq[Int]) =
    recurse[Int,Int](as)(_ max _)(_ getOrElse Int.MinValue)
  
  def totalWords(ps : List[String]) : Par[Int] =
    recurse[String,Int](ps.toIndexedSeq)(_ + _)(_.map(_.split(" ").length).getOrElse(0))
  
  // well of course there is the obvious implementation that is similar to map2's
  // but the book suggests using map2 to implement so...
  def map3[A,B,C,D](pa : Par[A], pb : Par[B], pc : Par[C])(f : (A,B,C) => D) : Par[D] = {
    val pf : Par[C => D] = map2(pa,pb)((a,b) => (c:C) => f(a,b,c))
    map2(pf,pc)(_(_))
  }
  
  def map4[A,B,C,D,E](pa:Par[A],pb:Par[B],pc:Par[C],pd:Par[D])(f:(A,B,C,D)=>E) : Par[E] = {
    val pcde : Par[C => D => E] = map2(pa,pb)((a,b) => (c:C) => (d:D) => f(a,b,c,d))
    val pde : Par[D => E] = map2(pcde,pc)(_(_))
    map2(pde, pd)(_(_))
  }
  
  def equal[A](e : ExecutorService)(p1 : Par[A], p2 : Par[A]) : Boolean =
    p1(e).get == p2(e).get
  
  def delay[A](fa: => Par[A]) : Par[A] = es => fa(es)
}

object Chapter7 {
  def main(args : Array[String]) = {
    import Par._
    val e : ExecutorService = Executors.newFixedThreadPool(1)
    
    val a = lazyUnit(42+1)
    println(equal(e)(a,fork(a)))
  }
}