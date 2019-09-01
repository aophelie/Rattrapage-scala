
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

    //def apply[A , B](a: A , b: B) = if (a.isInstanceOf) first(a) else second(b)

    def first[A](value:A): Or[A , Nothing] = new First[A](value)

    def second[B](value:B): Or[Nothing , B] = new Second[B](value)

    def bimap[A, B, C, D](f: A => C , g: B => D): Or[C, D] = Bifunctor.orBifunctor.bimap(Or.this )(f,g)

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
    implicit val orBifunctor: Bifunctor[Or] = new Bifunctor[Or] {
      def bimap[A, B, C, D](x: Or[A, B])(f: A => C , g: B => D)= x match {
        case First(a) => First(f(a))
        case Second(b) => Second(g(b))
      }
    }
  }


  def bimap[A, B, C, D](f: A => C , g: B => D): Or[C, D] = Bifunctor.orBifunctor.bimap(this)(f,g)


  /*******************************
    * REVOIR CETTE FONCTION ==> Dicuter avec prof
    */

  def convertToInt(str: String): Or[Int, String] = str match {
    case _ if(str.toInt) => First(str.toInt)
    case _ => Second(str)
  }


}
