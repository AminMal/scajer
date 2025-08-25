package scajer.core

import munit.FunSuite
import scajer.core.StrItr

class StrItrTest extends FunSuite {

  test("StrItr peek should return next character, not changing the position and cp") {
    val strItr = StrItr.init("This is a string")
    assertEquals(strItr.peek(), 'T')
    assertEquals(strItr.pos, 0)
    assertEquals(strItr.cp, -1)
  }

  test("StrItr peek should return Char 0 if string has no chars ahead") {
    val strItr = StrItr.init("")
    assertEquals(strItr.peek(), 0.toChar)
    assertEquals(strItr.pos, 0)
    assertEquals(strItr.cp, -1)
  }

  test("StrItr hasNext should return true if pos is not at the end") {
    val strItr = StrItr.init("This is a string")
    assertEquals(strItr.hasNext, true)
    strItr.advance(4)
    assertEquals(strItr.hasNext, true)
  }

  test("StrItr hasNext should return false when reached end of string") {
    val s      = "This is a string"
    val strItr = StrItr.init(s)
    assertEquals(strItr.hasNext, true)
    strItr.advance(s.length)
    assertEquals(strItr.hasNext, false)
  }

  test("StrItr checkpoint should save a checkpoint") {
    val strItr       = StrItr.init("This is a string")
    strItr.advance(3)
    strItr.checkpoint()
    val currPosition = strItr.pos
    strItr.advance(4)
    assertEquals(strItr.cp, currPosition) // not going forward with pos
  }

  test("StrItr checkpoint should save only one checkpoint at a time") {
    val strItr = StrItr.init("This is a string")
    strItr.checkpoint()
    assertEquals(strItr.cp, strItr.pos)
    interceptMessage[RuntimeException]("Cannot set more than one checkpoint at a time")(strItr.checkpoint())
  }

  test("StrItr commitCheckpoint should commit a checkpoint") {
    val strItr       = StrItr.init("This is a string")
    strItr.advance(4)
    strItr.checkpoint()
    val currPosition = strItr.pos
    strItr.advance(5)
    // do some process, make sure everything is alright
    strItr.commitCheckpoint()
    assertEquals(strItr.cp, -1)
  }

  test("StrItr resetCheckpoint should bring the position back to the checkpoint") {
    val strItr       = StrItr.init("This is a string")
    strItr.advance(4)
    strItr.checkpoint()
    val currPosition = strItr.pos
    strItr.advance(5)
    assertEquals(strItr.pos, 9)
    // realize something went wrong, or you wanna re-iterate that part again with a different approach (what I do when parsing strings)
    strItr.resetCheckpoint()
    assertEquals(strItr.cp, -1)
    assertEquals(strItr.pos, 4)
  }

  test("StrItr advance should shift the position by n") {
    val strItr = StrItr.init("This is a string")
    assertEquals(strItr.pos, 0)
    strItr.advance(8)
    assertEquals(strItr.pos, 8)
    strItr.advance()
    assertEquals(strItr.pos, 9)
  }

  test("StrItr should return next character, changing the position but not changing cp") {
    val strItr = StrItr.init("This is a string")
    assertEquals(strItr.pos, 0)
    assertEquals(strItr.pop(), 'T')
    assertEquals(strItr.pos, 1)
    assertEquals(strItr.cp, -1)
  }

  test("StrItr pop should return Char of 0 when has no next elements") {
    val strItr = StrItr.init("")
    assertEquals(strItr.pop(), 0.toChar)
    assertEquals(strItr.pos, 0)
    assertEquals(strItr.cp, -1)
  }

  test("StrItr startsWith should return whether the remaining substring at that position starts with the given string") {
    val strItr          = StrItr.init("This is a String")
    val `index of 'is'` = 5
    strItr.advance(`index of 'is'`)
    assertEquals(strItr.startsWith("is"), true)
    assertEquals(strItr.startsWith("as"), false)
  }

}
