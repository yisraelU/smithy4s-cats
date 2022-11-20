package smithy4s.cats

import cats.effect.IO
import cats.syntax.all._
import smithy4s.{ByteArray, Hints, ShapeId, Timestamp}
import smithy4s.schema.Schema
import smithy4s.schema.Schema._
import weaver._
import weaver.Expectations.Helpers.expect

object ShowCodecSpec extends FunSuite {

  val schemaVisitorShow = new SchemaVisitorShow()

  test("int") {
    val schema: Schema[Int] = int
    val intValue = 1
    val showOutput = schemaVisitorShow(schema).show(intValue)
    expect.eql(showOutput, "1")
  }

  test("string") {
    val schema: Schema[String] = string
    val foo = "foo"
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, "foo")
  }

  test("boolean") {
    val schema: Schema[Boolean] = boolean
    val foo = true
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, "true")
  }

  test("long") {
    val schema: Schema[Long] = long
    val foo = 1L
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, "1")
  }

  test("short") {
    val schema: Schema[Short] = short
    val foo = 1.toShort
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, "1")
  }

  test("byte") {
    val schema: Schema[Byte] = byte
    val foo = 1.toByte
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, "1")

  }

  test("double") {
    val schema: Schema[Double] = double
    val foo = 1.0
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, "1.0")

  }

  test("float") {
    val schema: Schema[Float] = float
    val foo = 1.0f
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, "1.0")
  }

  test("bigint") {
    val schema: Schema[BigInt] = bigint
    val foo = BigInt(1)
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, "1")
  }

  test("bigdecimal") {
    val schema: Schema[BigDecimal] = bigdecimal
    val foo = BigDecimal(1)
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, "1")

  }

  test("smithy4s ByteArray") {
    val schema: Schema[ByteArray] = bytes
    val fooBar = ByteArray("fooBar".getBytes)
    val showOutput = schemaVisitorShow(schema).show(fooBar)
    expect.eql(showOutput, "Zm9vQmFy")
  }

  test("smithy4s timestamp") {
    val schema: Schema[Timestamp] = timestamp
    val now = java.time.Instant.now()
    val foo = Timestamp.fromEpochSecond(now.getEpochSecond)
    val showOutput = schemaVisitorShow(schema).show(foo)
    expect.eql(showOutput, foo.toString)
  }

  test("struct") {
    case class Foo(x: String, y: Option[String])
    object Foo {
      val schema: Schema[Foo] = {
        StructSchema(
          ShapeId("", "Foo"),
          Hints.empty,
          Vector(
            string.required[Foo]("x", _.x),
            string.optional[Foo]("y", _.y)
          ),
          arr =>
            Foo.apply(
              arr(0).asInstanceOf[String],
              arr(1).asInstanceOf[Option[String]]
            )
        )

      }
    }
    val foo = Foo("foo", Some("bar"))
    val showOutput = schemaVisitorShow(Foo.schema).show(foo)
    expect.eql(showOutput, foo.toString)
  }
  test("struct: empty optional") {
    case class Foo(x: String, y: Option[String])
    object Foo {
      val schema: Schema[Foo] = {
        StructSchema(
          ShapeId("", "Foo"),
          Hints.empty,
          Vector(
            string.required[Foo]("x", _.x),
            string.optional[Foo]("y", _.y)
          ),
          arr =>
            Foo.apply(
              arr(0).asInstanceOf[String],
              arr(1).asInstanceOf[Option[String]]
            )
        )

      }
    }

    val foo = Foo("foo", None)
    val showOutput = schemaVisitorShow(Foo.schema).show(foo)
    expect.eql(showOutput, foo.toString)
  }

  test("list") {
    case class Foo(foos: List[Int])
    object Foo {
      val schema: Schema[Foo] = {
        val foos = list(int)
          .required[Foo]("foos", _.foos)
        struct(foos)(Foo.apply)
      }.withId(ShapeId("", "Foo"))
    }
    val foo = Foo(List(1, 2, 3))
    val showOutput = schemaVisitorShow(Foo.schema).show(foo)
    expect.eql(showOutput, foo.toString)
  }

  test("recursion") {
    case class Foo(foo: Option[Foo])
    object Foo {
      val schema: Schema[Foo] = recursive {
        val foos = schema.optional[Foo]("foo", _.foo)
        struct(foos)(Foo.apply)
      }.withId(ShapeId("", "Foo"))
    }

    val foo = Foo(Some(Foo(None)))
    val showOutput = schemaVisitorShow(Foo.schema).show(foo)
    expect.eql(showOutput, foo.toString)

  }

  test("union") {
    sealed trait IntOrString
    case class IntValue(value: Int) extends IntOrString
    case class StringValue(value: String) extends IntOrString
    val schema: Schema[IntOrString] = {
      val intValue = int.oneOf[IntOrString]("intValue", IntValue(_))
      val stringValue = string.oneOf[IntOrString]("stringValue", StringValue(_))
      union(intValue, stringValue) {
        case IntValue(int)     => intValue(int)
        case StringValue(string) => stringValue(string)
      }.withId(ShapeId("", "Foo"))
    }
    val foo0 =IntValue(1)
    val foo1 = StringValue("foo")
    val showOutput0 = schemaVisitorShow(schema).show(foo0)
    val showOutput1 = schemaVisitorShow(schema).show(foo1)
    expect.eql(showOutput0, "1")
    expect.eql(showOutput1, "foo")



  }

  test("enumeration") {
    sealed abstract class FooBar(val stringValue: String, val intValue: Int)
      extends smithy4s.Enumeration.Value {
      val name = stringValue
      val value = stringValue
      val hints = Hints.empty
    }
    object FooBar {
      case object Foo extends FooBar("foo", 0)
      case object Bar extends FooBar("bar", 1)
      val schema: Schema[FooBar] =
        enumeration[FooBar](List(Foo, Bar))
    }
    val foo = FooBar.Foo
    val showOutput = schemaVisitorShow(FooBar.schema).show(foo)
    val  bar = FooBar.Bar
    val showOutput1 = schemaVisitorShow(FooBar.schema).show(bar)
    expect.eql(showOutput, foo.stringValue)
    expect.eql(showOutput1, bar.stringValue)
  }
  /*

  test("map") {
    case class Foo(foos: Map[String, Int])
    object Foo {
      val schema: Schema[Foo] = {
        val foos = map(string, int)
          .required[Foo]("foos", _.foos)
          .addHints(XmlName("entries"))
        struct(foos)(Foo.apply)
      }
    }

    val xml = """|<Foo>
                 |   <entries>
                 |        <entry>
                 |            <key>a</key>
                 |            <value>1</value>
                 |        </entry>
                 |        <entry>
                 |            <key>b</key>
                 |            <value>2</value>
                 |        </entry>
                 |   </entries>
                 |</Foo>""".stripMargin
    checkContent(xml, Foo(Map("a" -> 1, "b" -> 2)))
  }*/

  /*test("map: custom names") {
    case class Foo(foos: Map[String, Int])
    object Foo {
      val schema: Schema[Foo] = {
        val foos =
          map(string.addHints(XmlName("k")), int.addHints(XmlName("v")))
            .required[Foo]("foos", _.foos)
        struct(foos)(Foo.apply)
      }
    }

    val xml = """|<Foo>
                 |   <foos>
                 |        <entry>
                 |            <k>a</k>
                 |            <v>1</v>
                 |        </entry>
                 |        <entry>
                 |            <k>b</k>
                 |            <v>2</v>
                 |        </entry>
                 |   </foos>
                 |</Foo>""".stripMargin
    checkContent(xml, Foo(Map("a" -> 1, "b" -> 2)))
  }*/

  /* test("map: flattened") {
    case class Foo(foos: Map[String, Int])
    object Foo {
      val schema: Schema[Foo] = {
        val foos =
          map(string, int)
            .required[Foo]("foos", _.foos)
            .addHints(XmlFlattened())
        struct(foos)(Foo.apply)
      }
    }*/

  /*    val xml = """|<Foo>
                 |   <foos>
                 |      <key>a</key>
                 |      <value>1</value>
                 |   </foos>
                 |   <foos>
                 |      <key>b</key>
                 |      <value>2</value>
                 |   </foos>
                 |</Foo>""".stripMargin
    checkContent(xml, Foo(Map("a" -> 1, "b" -> 2)))
  }*/
  /*
  test("Document decoding") {
    case class Foo(x: Int)
    object Foo {
      val schema: Schema[Foo] = {
        val x = int.required[Foo]("x", _)
        struct(x)(Foo.apply)
      }.withId(ShapeId("foo", "Foo"))
    }

    val xml = """|<Foo>
                 |   <x>1</x>
                 |</Foo>""".stripMargin
    checkDocument(xml, Foo(1))
  }*/

  /*  test("Document decoding: custom name") {
    case class Foo(x: Int)
    object Foo {
      val schema: Schema[Foo] = {
        val x = int.required[Foo]("x", _)
        struct(x)(Foo.apply).addHints(XmlName("F"))
      }
    }

    val xml = """|<F>
                 |   <x>1</x>
                 |</F>""".stripMargin
    checkDocument(xml, Foo(1))
  }*/

  /* test("Document decoding: failure") {
    case class Foo(x: Int)
    object Foo {
      val schema: Schema[Foo] = {
        val x = int.required[Foo]("x", _)
        struct(x)(Foo.apply)
      }.withId(ShapeId("foo", "Foo"))
    }

    val xml = """|<Bar>
                 |   <x>1</x>
                 |</Bar>""".stripMargin
    parseDocument(xml)
      .flatMap(decodeDocument[Foo](_))
      .attempt
      .map { result =>
        expect.same(
          result,
          Left(
            XmlDecodeError(XPath.root, "Expected Foo XML root element, got Bar")
          )
        )
      }
  }*/

}
