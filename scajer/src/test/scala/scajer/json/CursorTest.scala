package scajer
package json

import munit.FunSuite
import scajer.core.Parser

class CursorTest extends FunSuite {

  val Some(json) = Parser
    .parse("""
             |{
             |  "str": "String",
             |  "long": 12,
             |  "double": 12.4,
             |  "null": null,
             |  "true": true,
             |  "false": false,
             |  "obj": {
             |    "info": "Just testing"
             |  },
             |  "empty_arr": [],
             |  "arr": [1, true, {"title":"The lord of the rings"}]
             |}
             |""".stripMargin)
    .toOption: @unchecked

  // String
  test("Cursor.str") {
    (json.cursor ~> "str").str match {
      case s: String      => assertEquals(s, "String")
      case e: CursorError => fail(e.toString)
    }
  }

  test("Cursor.strOpt") {
    (json.cursor ~> "str").strOpt match {
      case Some(s) => assertEquals(s, "String")
      case None    => fail("expected value 'String', faced error")
    }
  }

  test("Cursor.getStr") {
    assertEquals((json.cursor ~> "str").getStr, "String")
  }

  test("Cursor.str on another type") {
    (json.cursor ~> "long").str match {
      case s: String      => fail("should have failed with invalid type")
      case e: CursorError =>
        assertEquals(e, CursorError.IncompatibleValueType("String", "Long", ".long"))
        assertEquals(e.toString, "cannot cast json value of type Long (at path = .long) to type String")
    }
  }

  // Long
  test("Cursor.long") {
    (json.cursor ~> "long").long match {
      case l: Long        => assertEquals(l, 12L)
      case e: CursorError => fail(e.toString)
    }
  }

  test("Cursor.longOpt") {
    (json.cursor ~> "long").longOpt match {
      case Some(l) => assertEquals(l, 12L)
      case None    => fail("expected value '12', faced error")
    }
  }

  test("Cursor.getLong") {
    assertEquals((json.cursor ~> "long").getLong, 12L)
  }

  test("Cursor.long on another type") {
    (json.cursor ~> "double").long match {
      case l: Long        => fail("should have failed with invalid type")
      case e: CursorError =>
        assertEquals(e, CursorError.IncompatibleValueType("Long", "Double", ".double"))
        assertEquals(e.toString, "cannot cast json value of type Double (at path = .double) to type Long")
    }
  }

  // Double
  test("Cursor.double") {
    (json.cursor ~> "double").double match {
      case d: Double      => assertEquals(d, 12.4d)
      case e: CursorError => fail(e.toString)
    }
  }

  test("Cursor.doubleOpt") {
    (json.cursor ~> "double").doubleOpt match {
      case Some(d) => assertEquals(d, 12.4d)
      case None    => fail("expected value '12.4', faced error")
    }
  }

  test("Cursor.getDouble") {
    assertEquals((json.cursor ~> "double").getDouble, 12.4d)
  }

  test("Cursor.double on another type") {
    (json.cursor ~> "true").double match {
      case d: Double      => fail("should have failed with invalid type")
      case e: CursorError =>
        assertEquals(e, CursorError.IncompatibleValueType("Double", "Boolean", ".true"))
        assertEquals(e.toString, "cannot cast json value of type Boolean (at path = .true) to type Double")
    }
  }

  // Boolean
  test("Cursor.bool") {
    (json.cursor ~> "true").bool match {
      case b: Boolean     => assertEquals(b, true)
      case e: CursorError => fail(e.toString)
    }
  }

  test("Cursor.boolOpt") {
    (json.cursor ~> "false").boolOpt match {
      case Some(b) => assertEquals(b, false)
      case None    => fail("expected value 'false', faced error")
    }
  }

  test("Cursor.getBool") {
    assertEquals((json.cursor ~> "true").getBool, true)
    assertEquals((json.cursor ~> "false").getBool, false)
  }

  test("Cursor.bool on another type") {
    (json.cursor ~> "long").bool match {
      case b: Boolean     => fail("should have failed with invalid type")
      case e: CursorError =>
        assertEquals(e, CursorError.IncompatibleValueType("Boolean", "Long", ".long"))
        assertEquals(e.toString, "cannot cast json value of type Long (at path = .long) to type Boolean")
    }
  }

  test("Cursor.access non-existing path") {
    val funcs: List[Cursor => CursorError | Any] = List(
      _.str,
      _.long,
      _.double,
      _.bool,
      _.value
    )

    funcs
      .map(f => f(json.cursor ~> "non-existing-path"))
      .foreach {
        case e: CursorError =>
          assertEquals(e, CursorError.PathDoesNotExist(".non-existing-path"))
          assertEquals(e.toString, "json path does not exist: .non-existing-path")
        case _              => fail("should have failed with proper CursorError")
      }
  }

  // not an object
  test("Cursor trying to access non-object AST as object") {
    (json.cursor ~> "arr" ~> 1 ~> "title").value match {
      case v: JsValue     => fail("should have failed with proper CursorError")
      case e: CursorError =>
        assertEquals(e, CursorError.NotAnObject(".arr.1"))
        assertEquals(e.toString, "value at `.arr.1` is not an object")
    }
  }

  // not an array
  test("Cursor trying to access non-array AST as array") {
    (json.cursor ~> "true" ~> 1).value match {
      case v: JsValue     => fail("should have failed with proper CursorError")
      case e: CursorError =>
        assertEquals(e, CursorError.NotAnArray(".true"))
        assertEquals(e.toString, "value at `.true` is not an array")
    }
  }

  // index out of bounds
  test("Cursor trying to access non-array AST as array") {
    (json.cursor ~> "arr" ~> 986).value match {
      case v: JsValue     => fail("should have failed with proper CursorError")
      case e: CursorError =>
        assertEquals(e, CursorError.IndexOutOfBounds(".arr.986", 3))
        assertEquals(e.toString, "json array index out of bounds.\n\t.arr.986\n\tarray size: 3")
    }
  }
}
