object Main extends App {


  sealed abstract class Or[A , B] extends Product with Serializable {
    self =>

    def isEmpty: Boolean

  }


  final case class First[A](value: A) extends Or[A,_] {
    def get: Class[_ <: A] = value.getClass // retourner le type des éléments de la classe ??? Demander plus d'explications

    override def isEmpty: Boolean = false
  }


  final case class Second[A](value: A) extends Or[_,A] {
    def get: Class[_ <: A] = value.getClass

    override def isEmpty: Boolean = false
  }


  /*******************************
    * Campanion object
    */
  object Or {

    def apply[A , B](a: A , b: B): Any = if (a == null) second(b) else first(a)

    def empty[A , B] = ??? //new Or[A, B](NoneValue, NoneValue)

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
    def apply[A](value:A) = Or.second(value)
    //def unapply[A](value:Or[_,A]) = ???
  }


  /*******************************
    * Trait BiFunctor
    */
  trait Bifunctor[F[_ , _]] extends Serializable{ self =>

    def bimap[A, B, C, D](fab: F[A , B])(f: A => C , g: B => D): F[C , D]

  }


  /*******************************
    * REVOIR CETTE FONCTION ==> Dicuter avec prof
    */

  def convertToInt(str: String): Or[String,Int] = str match {
      case _  if(str.toInt) => Second(str)
      case _ => First(str)
  }


}
