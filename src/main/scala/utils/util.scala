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

  def fold[A,B] (list:List[A], zero:B, f:(B,A) => B):B = list match {
  	case Nil => zero
  	case x :: xs => f(fold(xs, zero, f), x)
  }

}