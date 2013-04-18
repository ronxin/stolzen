package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), 
    			  Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("addToFreqs to a list of pairs, without a char") {
    val list = List(('t', 2), ('e', 1), ('x', 3))
    assert(addToFreqs('y', list) == List(('t', 2), ('e', 1), ('x', 3), ('y', 1)))
  }

  test("addToFreqs to a list of pairs, with a char") {
    val list = List(('t', 2), ('e', 1), ('x', 3))
    assert(addToFreqs('e', list) == List(('t', 2), ('e', 2), ('x', 3)))
  }

  test("times for \"hello, world\"") {
    val actual = times(string2Chars("hello, world"))
    val expected = List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1))
    assert(actual == expected)
  }

  test("toLeaves") {
    val testData = List(('h', 1), ('l', 3), ('o', 2), ('r', 1), ('d', 1))
    val expected = List(Leaf('h', 1), Leaf('l', 3), Leaf('o', 2), Leaf('r', 1), Leaf('d', 1))
    val actual = toLeaves(testData)
    assert(actual == expected)
  }

  trait SortingTestData {
    val testData = List(Leaf('r', 3), Leaf('l', 4), Leaf('o', 6), Leaf('h', 8))
  }

  test("insertToSorted for sortByWeight, first") {
    new SortingTestData {
      val actual = insertToSorted(Leaf('t', 1), testData)
      val expected = List(Leaf('t', 1), Leaf('r', 3), Leaf('l', 4), Leaf('o', 6), Leaf('h', 8))
      assert(actual == expected)
    }
  }

  test("insertToSorted for sortByWeight, middle") {
    new SortingTestData {
      val actual = insertToSorted(Leaf('t', 5), testData)
      val expected = List(Leaf('r', 3), Leaf('l', 4), Leaf('t', 5), Leaf('o', 6), Leaf('h', 8))
      assert(actual == expected)
    }
  }

  test("insertToSorted for sortByWeight, last") {
    new SortingTestData {
      val actual = insertToSorted(Leaf('t', 10), testData)
      val expected = List(Leaf('r', 3), Leaf('l', 4), Leaf('o', 6), Leaf('h', 8), Leaf('t', 10))
      assert(actual == expected)
    }
  }

  test("sortByWeight") {
    val testData = List(Leaf('o', 2), Leaf('r', 4), Leaf('h', 1), Leaf('l', 3))
    val expected = List(Leaf('h', 1), Leaf('o', 2), Leaf('l', 3), Leaf('r', 4))
    val actual = sortByWeight(testData)
    assert(actual == expected)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("signleton") {
    assert(singleton(List(Leaf('o', 2), Leaf('r', 4))) == false)
    assert(singleton(List()) == false)
    assert(singleton(List(Leaf('o', 2))) == true)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine for empty list") {
    assert(combine(Nil) == Nil)
  }

  test("combine for one-element list") {
    val list = List(Leaf('e', 1))
    assert(combine(list) == list)
  }

  test("make code tree") {
    val expected = Fork(Leaf('e', 1), Leaf('t', 2), string2Chars("et"), 3)
    val actual = makeCodeTree(Leaf('e', 1), Leaf('t', 2))
    assert(actual == expected)
  }

  test("combine for 2-element list") {
    val expected = makeCodeTree(Leaf('e', 1), Leaf('t', 2))
    val actual = combine(List(Leaf('e', 1), Leaf('t', 2)))
    assert(actual.head == expected)
  }

  trait TreeTestData {
    val alphabet = string2Chars("aabbbbcccccdddddddd")
    val leaves = List(Leaf('a', 2), Leaf('b', 4), Leaf('c', 5), Leaf('d', 8))
    val expectedTree = Fork(Leaf('d', 8),
					Fork(Leaf('c', 5),
						 Fork(Leaf('a', 2),
						      Leaf('b', 4),
						      string2Chars("ab"), 6),
						 string2Chars("cab"), 11),
					string2Chars("dcab"), 19)
    val message = string2Chars("abcdabba")
    val codedMessage = List(1,1,0, 1,1,1, 1,0, 0, 1,1,0, 1,1,1, 1,1,1, 1,1,0)  
  }

  test("until(singleton, combine)(trees)") {
    new TreeTestData {
      val actual = until(singleton, combine)(leaves)
      assert(actual.head == expectedTree)
    }
  }

  test("createCodeTree") {
    new TreeTestData {
      val actualTree = createCodeTree(alphabet)
      assert(actualTree == expectedTree)
    }
  }

  test("decode for TreeTestData") {
    new TreeTestData {
      val actualDecodedMessage = decode(expectedTree, codedMessage)
      assert(actualDecodedMessage == message)
    }
  }

  test("decode for frenchCode") {
    assert(decode(frenchCode, secret) == string2Chars("huffmanestcool"))
  }

  test("member") {
    assert(member('c', string2Chars("abcd")))
    assert(!member('f', string2Chars("abcd")))
  }

  test("encode for TreeTestData") {
    new TreeTestData {
      val actualCode = encode(expectedTree)(message)
      assert(actualCode == codedMessage)
    }
  }

  test("endoce for frenchCode") {
    assert(encode(frenchCode)(string2Chars("huffmanestcool")) == secret)
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("quickEncode for TreeTestData") {
    new TreeTestData {
      val actualCode = quickEncode(expectedTree)(message)
      assert(actualCode == codedMessage)
    }
  }

  test("quickEncode for frenchCode") {
    assert(quickEncode(frenchCode)(string2Chars("huffmanestcool")) == secret)
  }  
  
}
