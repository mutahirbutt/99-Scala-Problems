import scala.annotation.tailrec

//Problem 1: Find the last element of a list
@tailrec
def lastElem[A](l: List[A]): Option[A] = l match {
    case Nil       => None
    case x :: Nil  => Some(x)
    case _ :: xs   => lastElem(xs) // l.reverse.head

}


//Problem 2: Find the last-but-one (or second-last) element of a list

@tailrec
def secondlastElem[A](l: List[A]): Option[A] = l match {
    case Nil                 => None
    case x :: y :: z :: Nil  => Some(y)
    case _ :: xs             => secondlastElem(xs)

}