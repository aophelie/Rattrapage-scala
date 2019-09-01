

object Main extends App {


  sealed abstract class Or[+A , +B] {

  }


  final case class First[A](value: A) extends Or[A , Nothing] {

  }


  final case class Second[B](value: B) extends Or[Nothing , B] {

  }


  /*******************************
    * Campanion object
    */
  object Or {

    def first[A](value:A): Or[A , Nothing] = new First[A](value)

    def second[B](value:B): Or[Nothing , B] = new Second[B](value)

  }


  /*******************************
    * Campanion object
    */
  object First {
    def apply[A](value:A) = Or.first(value)
  }


  /*******************************
    * Campanion object
    */
  object Second {
    def apply[B](value:B) = Or.second(value)
  }


  /*******************************
    * Trait BiFunctor
    */
  trait Bifunctor[F[_ , _]] {

    def bimap[A, B, C, D](fab: F[A,B])(f: A => C , g: B => D): F[C,D]

  }


  object Bifunctor {
    implicit val OrBifunctor: Bifunctor[Or] = new Bifunctor[Or] {
      def bimap[A, B, C, D](x: Or[A, B])(f: A => C , g: B => D)= x match {
        case First(a) => First(f(a))
        case Second(b) => Second(g(b))
      }
    }
  }


  object Implicits {

    implicit class OrPlus[A, B](x: Or[A, B]) {
      def bimap[A, B, C, D](f: A => C)(g: B => D) = Bifunctor.OrBifunctor.bimap(x)(f,g)
    }

    /*implicit class BiMap[F[_, _], A, B](inner: F[A, B]) {
      def bimap[A, B, C, D](fac: A => C)(fbd: B => D) = {
        implicitly[Bifunctor[F]].bimap(inner)(fac)(fbd)
      }
    }*/

  }




  /*******************************
    * REVOIR CETTE FONCTION ==> Dicuter avec prof
    */

  import scala.util.Try

  def convertToInt(str: String): Or[Int, String] = str match {
    case _ if Try(str.toInt).isSuccess => First(str.toInt)
    case _ => Second(str)
  }


}
