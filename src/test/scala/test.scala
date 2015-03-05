package NinetyNineProblems

import utest._
import utest.ExecutionContext.RunNow

object Tests extends TestSuite{
  val tests = TestSuite{
    'reverse{
      assert(List(1,2,3) == Util.reverse(List(3,2,1)))
    }

  }
}