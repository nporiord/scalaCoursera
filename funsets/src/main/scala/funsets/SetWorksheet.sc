package funsets


object SetWorksheet {

	type Set = Int => Boolean
	
	def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: Int => Boolean, elem: Int)Boolean
	def singletonSet(elem: Int): Set = (x: Int) => x == elem
                                                  //> singletonSet: (elem: Int)Int => Boolean
		
	def allNumbersSet(): Set = (x: Int) => true
                                                  //> allNumbersSet: ()Int => Boolean
		
	def union(s: Set, t:Set): Set = (x: Int) => s(x) || t(x)
                                                  //> union: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
	def intersect(s: Set, t:Set): Set = (x: Int) => s(x) && t(x)
                                                  //> intersect: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
	def diff(s: Set, t:Set): Set = (x: Int) => s(x) && !t(x)
                                                  //> diff: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
	
	def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) & p(x)
                                                  //> filter: (s: Int => Boolean, p: Int => Boolean)Int => Boolean
	
	def forall(s: Set, p: Int => Boolean): Boolean = {
 		def iter(a: Int): Boolean = {
 			//If we've got all the way through all the integers, then everything matches.
   		if (a > 1000) true
   		//If one item is in the set, but fails the predicate function, then we short circuit and return.
   		else if (contains(s, a) && !p(a)) false
   		//otherwise we check the next one.
   		else iter(a + 1)
 		}
 		iter(-1000)
	}                                         //> forall: (s: Int => Boolean, p: Int => Boolean)Boolean
	
	
	var fullSet = union(union(singletonSet(1), singletonSet(2)), union(singletonSet(3), singletonSet(4)))
                                                  //> fullSet  : Int => Boolean = <function1>
	
	forall(fullSet, (x) => x > 0 && x < 5)    //> res0: Boolean = true
	
	def exists(s: Set, p: Int => Boolean) : Boolean =
		!forall(s, (x) => !p(x))          //> exists: (s: Int => Boolean, p: Int => Boolean)Boolean
		
	exists(fullSet, (x) => x > 0 && x < 5)    //> res1: Boolean = true
	
	
	val temp = exists(fullSet, (x) => x == x) //> temp  : Boolean = true

	
}