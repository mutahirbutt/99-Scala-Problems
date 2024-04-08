import scala.annotation.tailrec

//Problem 1: Find the last element of a list
//Example: lastElem(List(1,2,3)) => Some(3)
@tailrec
def lastElem[A](l: List[A]): Option[A] = l match {
    case Nil       => None
    case x :: Nil  => Some(x)
    case _ :: xs   => lastElem(xs) // l.reverse.head

}


//Problem 2: Find the last-but-one (or second-last) element of a list
//Example: secondlastElem(List(1,2,3)) => Some(2)
@tailrec
def secondlastElem[A](l: List[A]): Option[A] = l match {
    case Nil                 => None
    case x :: y :: z :: Nil  => Some(y)
    case _ :: xs             => secondlastElem(xs)

}

//Problem 3: Find the K'th element of a list
//Example: KthElem(List(1,2,3),2) => Some(3)
@tailrec
def KthElem[A](l: List[A], k: Int): Option[A] = (l,k) match {
    case (Nil,_) => None 
    case (_, n) if (n > l.size - 1) => None
    case ((x::_), 0) => Some(x)
    case ((_::xs), n) => KthElem(xs, n - 1) // Some(l(k))

}

//Problem 4: Find the number of elements in a list
//Example: lengthList(List(1,2,3)) => 3
def lengthList[A](l: List[A]): Int = {
    @tailrec
    def loop(l:List[A], acc: Int): Int = l match {
        case Nil => acc
        case (x::xs) => loop(xs, acc + 1) 

    }
    loop(l, 0)
}


//Problem 5: Reverse a list
def reverseList[A](l: List[A]): List[A] = {
    l.foldLeft(List.empty)((a,b) => b :: a)
}

def recursiveReverse[A](l: List[A]): List[A] = {
    @tailrec
    def loop(l: List[A], acc: List[A]): List[A] = l match {
        case Nil     => acc
        case x :: xs => loop(xs, x :: acc)
    }
    loop(l, List.empty)
}


//Problem 6: Find out whether a list is a palindrome 
//a palindrome can be read forward or backward

def isPalindrome[A](l: List[A]): Boolean = {
    l == reverseList(l)
}

//Problem 7: Flatten a nested list structure
//Example: 
//flattenNest(List(List(1),2,List(2,List(3,4)),5)) => 
//List(1,2,2,3,4,5)


def flattenNest(xs: List[Any]): List[Any] = {
    if xs.nonEmpty then xs.head match {
        case ys: List[Any] => flattenNest(ys) ++ flattenNest(xs.tail)
        case y => y :: flattenNest(xs.tail) 
    }
    else Nil 

}

//using an accumulator
def flattenRec(xs: List[Any]): List[Any] = {
    def loop(xs: List[Any], acc: List[Any]): List[Any] = {
        xs match {
            case Nil => acc
            case (x::xs) => x match {
                case ys: List[Any] => loop(xs,loop(ys,acc))
                case y => loop(xs, y :: acc)
            }
        }
    }
    loop(xs, List.empty).reverse
}

//using a for-comprehension
def flattenNest(xs: List[Any]): List[Any] = {
    for {
        x <- xs
        y <- x match {
            case ys: List[Any] => flattenNest(ys)
            case y => List(y)
        }
    } yield y
}

//Problem 8: Eliminate consecutive duplicates of list elements
//compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
//=> List('a', 'b', 'c', 'a', 'd', 'e')

def compress[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs => x :: compress(xs.dropWhile(_== x))

}

//Problem 9: Pack consecutive duplicates of list elements into sublists
//pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
//List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e'))

def pack[A](l: List[A]): List[List[A]] = l match {
    case Nil => Nil
    case x :: xs => List(l.takeWhile(_ == x)) ::: pack(l.dropWhile(_ == x))
}