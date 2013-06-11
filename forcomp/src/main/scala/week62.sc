package week6

import forcomp.Anagrams._

object anagramShenaigan {

	val w1 = new Word("a")
	val w2 = new Word("bb")
	val w3 = new Word("ccc")
	val w4 = new Word("dddd")
	val w5 = new Word("eeeee")
	val s = List(w1,w2,w3,w4,w5)
  
  s.foldLeft("")(_+_)
  
  sentenceOccurrences(s)
                                                  
 
  
 val smallDict = s.groupBy(w => wordOccurrences(w))
  val tmp = wordOccurrences("gib")
 
 def wordAnagrams(word: Word): List[Word] = {
   smallDict(wordOccurrences(word))
 }
 
 wordAnagrams("eeeee")
 
 def subSetPerLetter(pair: (Char, Int), holding: Occurrences): Occurrences = {
 	if(pair._2 == 0)
 		holding
  else
    subSetPerLetter((pair._1 , pair._2 -1), pair :: holding)
 }

  val alist = subSetPerLetter(sentenceOccurrences(s).head, Nil)
  val blist = subSetPerLetter(sentenceOccurrences(s).tail.head, Nil)
  val clist = subSetPerLetter(sentenceOccurrences(s).tail.tail.head, Nil)
 
 
  def listProduct1(alist: Occurrences, blist:  Occurrences) = {
  //first part
    for(a <- alist; b <- blist)
      yield List(a, b)
  }
  
  def listProduct2(alist: Occurrences) = {
  //first part
    for(a <- alist)
      yield List(a)
  }
 
 def simpleCombo(alist: Occurrences, blist:  Occurrences) = {
   listProduct1(alist, blist) ++ listProduct2(alist) ++ listProduct2(blist)
   
 }
}