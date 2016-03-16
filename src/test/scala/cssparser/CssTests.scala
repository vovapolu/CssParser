package cssparser

import utest._
import fastparse.all._
import Ast._

import scala.collection.mutable.ArrayBuffer

object CssTests extends TestSuite {
  val tests = this {
    'basic {
      'test1 {
        val Parsed.Success(value1, index1) = CssParser.stylesheet.parse(
          """
            |
            |p > a {
            |    color: blue;
            |    text-decoration: underline;
            |  }
            |
          """.stripMargin)

        assert(
          value1 ==
            Stylesheet(ArrayBuffer(
              Left(QualifiedRule(ArrayBuffer(
                  IdentToken("p"),
                  DelimToken(">"),
                  IdentToken("a")),
                CurlyBracketsBlock(ArrayBuffer(
                  IdentToken("color"),
                  DelimToken(":"),
                  IdentToken("blue"),
                  DelimToken(";"),
                  IdentToken("text-decoration"),
                  DelimToken(":"),
                  IdentToken("underline"),
                  DelimToken(";"))))))),
          index1 == 74
        )
      }
    }
  }
}
