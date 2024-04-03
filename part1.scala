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

//Find the K'th element of a list

@tailrec
def KthElem[A](l: List[A], k: Int): Option[A] = (l,k) match {
    case (Nil,_) => None 
    case (_, n) if (n > l.size - 1) => None
    case ((x::_), 0) => Some(x)
    case ((x::xs), n) => KthElem(xs, n - 1) // Some(l(k))

}