object Main extends App {

  sealed abstract class Or[A, B] extends Product with Serializable {
    self =>

    def isEmpty: Boolean

  }


  final case class First[A](value: A) extends Or[A,_] {
    def get: Class[_ <: A] = value.getClass // retourner le type des éléments de la classe

    override def isEmpty: Boolean = false
  }


  final case class Second[A](value: A) extends Or[_,A] {
    def get: Class[_ <: A] = value.getClass

    override def isEmpty: Boolean = false
  }


  object Or {

    def apply[A, B](a: A, b: B): Any = if (a == null) second(b) else first(a)

    def empty[A, B] = ??? //new Or[A, B](NoneValue, NoneValue)

    def first[A](value:A) = new First[A](value)

    def second[B](value:B) = new Second[B](value)

  }


  /*object First {
    def apply[T](value:T) = Or.first(value)
    //def unapply[T](value:Or[T]) = if(value.isDefined) scala.Some(value.get) else scala.None
  }


  object Second {
    def apply[T](value:T) = Or.second(value)
    //def unapply[T](value:Or[T]) = if(value.isDefined) scala.Some(value.get) else scala.None
  }*/


  trait Bifunctor[F[_, _]] extends Serializable{ self =>

    def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  }


  def convertToInt(str: String): Or[String,Int] = str match {
      case _  => First[String](str)
      case _ => Second[Int]
  }


}
