package NinetyNineProblems

object Main extends App {


	// println(Util.reverse(List(1, 1, 2, 3, 5, 2, 2222)))
	// println(Util.simpleFlatten(List(List(1,2,3), List(4,5,6), 3)))

	// println(Util.flatten(List(List(1,List(45,44,2),3), List(4,5,6), 3)))

	def myFlatMap(list: List[Int], f: Int => List[Int]): List[Int] =
		Util.fold[Int, List[Int]](List(1,5,8,2), Nil,
		(flattenedAlready, elem) => flattenedAlready ::: f(elem)
	)

	println(myFlatMap(List(1,2,3), x => List(x, x)))


}

