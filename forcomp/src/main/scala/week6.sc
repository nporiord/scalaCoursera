package week6

import forcomp.Anagrams._
import scala.collection.immutable.IndexedSeq

object anagramShenaigan {

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val occ = sentenceOccurrences(sentence)
    val com = combinations(occ)
    //recurse through dbo and build lists
    val dict = dictionaryByOccurrences
    
    subAnagrams(occ, com, dict, words);
    
  }
}