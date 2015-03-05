package NinetyNineProblems

import utest._
import utest.ExecutionContext.RunNow

object Tests extends TestSuite{
  val tests = TestSuite{
    'reverse{
      assert(List(1,2,3) == Util.reverse(List(3,2,1)))
    }
    'isPrime{
    	assert(Util.isPrime(6) == false)
    	assert(Util.isPrime(11) == true)
    	assert(Util.isPrime(21) == false)
    	assert(Util.isPrime(23) == true)
    }
    'isCoprimeTo{
      import Util.IntOps
      assert(6.isCoprimeTo(4) == false)
      assert(4.isCoprimeTo(9) == true)
    }

  }
}