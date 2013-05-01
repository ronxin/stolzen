package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  def wo = wordOccurrences(_)
  
  test("wo: abcd") {
    assert(wo("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wo: Robert") {
    assert(wo("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: Robert abcd") {
    val actual = sentenceOccurrences(List("Robert", "abcd"))
    val expected = List(('a', 1), ('b', 2), ('c', 1), ('d', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1))
    assert(expected === actual)
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }



  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }



  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: rrbb - rb") {
	val rbbr = wo("rbbr")
	val sub = wo("rb")
	val expected = wo("rb")
	
	val actual = subtract(rbbr, sub) 
	
    assert(actual === expected)
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: a x 3") {
    val a = List(('a', 3))

    val expected = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('a', 3))
    )

    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    val actual = combinations(abba)
    assert(actual.toSet === abbacomb.toSet)
  }



  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }  
  
  test("sentence anagrams: Heather") {
    val actual = sentenceAnagrams(List("Heather"))
    println(actual.toSet.size)
    println(actual.toSet)
  }
  
  test("findGoodOccurences when only one element") {
    val all = List(wo("r"))
    val toFind = wo("r")
    
    val expected = List(List(wo("r")))
    val result = findGoodOccurences(toFind, all)
    assert(result === expected)
  }

  test("findGoodOccurences when several element") {
    val all = List(wo("abcd"), wo("rbbr"), wo("rb"), wo("br"))
    val toFind = wo("bbrr")
    val result = findGoodOccurences(toFind, all)
    val expected = List(List(wo("rbbr")), List(wo("rb"), wo("br")))

    assert(result.toSet === expected.toSet)
  }
  
  test("good for") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    assert(goodFor(lard)(r))
  }

  test("not good for - doesn't contain") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('g', 1))
    assert(!goodFor(lard)(r))
  }

  test("not good for - contains, but not enough") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 2))
    assert(!goodFor(lard)(r))
  }
}
