
def sum(xs: List[Int]): Int = {
  def sumIter(xs: List[Int], sum: Int): Int = if (xs.isEmpty) sum else sumIter(xs.tail, sum + xs.head)
  sumIter(xs, 0)
}

def max(xs: List[Int]): Int = {
  def maxIter(xs: List[Int], max: Int): Int = if (xs.isEmpty) max else if (xs.head > max) maxIter(xs.tail, xs.head) else maxIter(xs.tail, max)
  if (xs.isEmpty) throw new NoSuchElementException else maxIter(xs, Int.MinValue)
}

sum(List(1,2,4,5,5,6,6,7,7,1,8,8,8,8,8,8))

max(List(Int.MinValue,-1,-2,-4,Int.MaxValue,5,6,6,7,7,1,8,8,8,8,8,8))

max(List(0))

