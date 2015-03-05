package NinetyNineProblems

object Util {

	def reverse (list:List[Int]):List[Int] = {
		def reverse0 (list:List[Int], reversedList:List[Int]):List[Int] = list match {
			case Nil => reversedList
			case x :: xs => reverse0(xs, x :: reversedList)
		}
		reverse0(list, Nil)
	}

	def flatten (list:List[Any]):List[Int] = {
		flatMap (
			list, 
			(x: Any) => x match {
				case x:Int => List(x)
				case x:List[Any] => flatten(x)
			}
		)
	}

	def flatMap[A,B] (list:List[A], f:A => List[B]):List[B] = {
		def flatMap0 (list:List[A], flatMapList:List[B]):List[B] = list match {
			case Nil => flatMapList
			case x :: xs => flatMap0(xs, flatMapList ::: f(x))
		}
		flatMap0(list,Nil)
	}

	def myFlatMap(list: List[Int], f: Int => List[Int]): List[Int] =
		Util.fold[Int, List[Int]](List(1,5,8,2), Nil,
		(flattenedAlready, elem) => flattenedAlready ::: f(elem)
	)

  def fold[A,B] (list:List[A], zero:B, f:(B,A) => B):B = list match {
  	case Nil => zero
  	case x :: xs => f(fold(xs, zero, f), x)
  }

  // P-16 Drop every Nth element from a list.
  def dropNthElement (n:Int, list:List[Any]):List[Any] = {
  	list.zipWithIndex.collect {
			case (x, i) if i % n != 0 => x
	  }
  }

  // P31 Determine whether a given integer number is prime.
  def isPrime (n:Int):Boolean = {
  	def isPrime0(k:Int):Boolean = k match {
  		case k if k == 1 => true
  		case k if n % k == 0 => false
  		case k => isPrime0(k-1)
  	}
  	isPrime0(n-1)
  }

  // P33 (*) Determine whether two positive integer numbers are coprime.
	implicit class IntOps(x:Int) {

		def isCoprimeTo(y:Int):Boolean = {
			def isCoprimeTo0(count:Int):Boolean = count match {
				case count if count == 1 => true
				case count if (x % count == 0 && y % count == 0) => false
				case count => isCoprimeTo0(count-1)
			}
			isCoprimeTo0(x.min(y))
		}
	}
}
