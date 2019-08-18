import simulacrum.typeclass

trait Bifunctor[F[_, _]] { self =>

  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def rightFunctor[X]: Functor[F[X, *]] =
    new RightFunctor[F, X] { val F = self }

  def leftFunctor[X]: Functor[F[*, X]] =
    new LeftFunctor[F, X] { val F = self }


  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)

  /** The composition of two Bifunctors is itself a Bifunctor */
  def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifunctor[F, G] {
      val F = self
      val G = G0
    }


  def leftWiden[A, B, AA >: A](fab: F[A, B]): F[AA, B] = fab.asInstanceOf[F[AA, B]]
}





trait Trait[A] {
  def traitType: String
}

object Trait {
  implicit val stringTrait: Trait[String] = new Trait[String] {
    def traitType: String = "string"
  }

  implicit val intTrait: Trait[Int] = new Trait[Int] {
    def traitType: String = "int"
  }
}

class Media[A] {
  // works
  def mediaType(implicit t: Trait[A]): String = t.traitType
  // does not compile
  def mediaType: String = implicitly[Trait[A]].traitType
}

object Main {
  def main(args: Array[String]) {
    val a = new Media[String]
    val b = new Media[Int]

    println(a.mediaType)
    println(b.mediaType)
  }
}



/////////////////////
/////////////////////////////
///////////////////////////////////
object Main extends App {


  sealed abstract class Or[A , B] {
    self =>

    //def isEmpty: Boolean

  }


  final case class First[A](value: A) extends Or[A , Nothing] {
    //def get: Class[_ <: A] = value.getClass // retourner le type des éléments de la classe ??? Demander plus d'explications

    //override def isEmpty: Boolean = false
  }


  final case class Second[B](value: B) extends Or[Nothing , B] {
    //def get: Class[_ <: B] = value.getClass

    //override def isEmpty: Boolean = false
  }


  /*******************************
    * Campanion object
    */
  object Or {

    def apply[A , B](a: A , b: B): Any = if (a == null) second(b) else first(a)

    //def empty[A , B] = ??? //new Or[A, B](NoneValue, NoneValue)

    def first[A](value:A) = new First[A](value)

    def second[B](value:B) = new Second[B](value)

  }


  /*******************************
    * Campanion object
    */
  object First {
    def apply[A](value:A) = Or.first(value)
    //def unapply[A](value:Or[A,_]) = ???
  }


  /*******************************
    * Campanion object
    */
  object Second {
    def apply[B](value:B) = Or.second(value)
    //def unapply[A](value:Or[_,A]) = ???
  }


  /*******************************
    * Trait BiFunctor
    */
  trait Bifunctor[F[_ , _]] {//extends Serializable{ self =>

    //def bimap[A, B, C, D](fab: F[A , B])(f: A => C , g: B => D): F[C , D]
    def bimap[A, B, C, D](fa: F[A,B])(f: (A,B) => (C,D)): F[C,D]

  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }


  /*******************************
    * REVOIR CETTE FONCTION ==> Dicuter avec prof
    */

  def convertToInt(str: String): Or[String,Int] = str match {
    case _  if(str.toInt) => Second(str)
    case _ => First(str)
  }


}
