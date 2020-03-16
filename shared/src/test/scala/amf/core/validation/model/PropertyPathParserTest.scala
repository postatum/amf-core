package amf.core.validation.model

import amf.core.unsafe.PlatformSecrets
import org.scalatest.AsyncFunSuite

import scala.concurrent.ExecutionContext

class PropertyPathParserTest extends AsyncFunSuite with PlatformSecrets {

  override implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  val expander: String => String = (s) => s"prefix::$s"

  test("Simple path") {
    assert(PropertyPathParser("a", expander) == PredicatePath(property = "prefix::a"))
  }

  test("Inverse path") {
    assert(PropertyPathParser("a^", expander) == PredicatePath(property = "prefix::a", inverse = true))
  }

  test("Sequence path") {
    assert(PropertyPathParser("a / b", expander) == SequencePath(
      List(
        PredicatePath(property = "prefix::a"),
        PredicatePath(property = "prefix::b")
      )
    ))
  }

  test("Sequence path last zeroOrMore") {
    assert(PropertyPathParser("a/b*", expander) == SequencePath(
      List(
        PredicatePath(property = "prefix::a"),
        PredicatePath(property = "prefix::b", zeroOrMore = true)
      )
    ))
  }

  test("Sequence path first zeroOrMore") {
    assert(PropertyPathParser("a*/b", expander) == SequencePath(
      List(
        PredicatePath(property = "prefix::a", zeroOrMore = true),
        PredicatePath(property = "prefix::b")
      )
    ))
  }

  test("Sequence path both zeroOrMore") {
    assert(PropertyPathParser("a*/b*", expander) == SequencePath(
      List(
        PredicatePath(property = "prefix::a", zeroOrMore = true),
        PredicatePath(property = "prefix::b", zeroOrMore = true)
      )
    ))
  }

  test("Sequence path both inverse") {
    assert(PropertyPathParser("a^/b^", expander) == SequencePath(
      List(
        PredicatePath(property = "prefix::a", inverse = true),
        PredicatePath(property = "prefix::b", inverse = true)
      )
    ))
  }

  test("Alternate path") {
    assert(PropertyPathParser("a|b", expander) == AlternatePath(
      List(
        PredicatePath(property = "prefix::a"),
        PredicatePath(property = "prefix::b")
      )
    ))
  }

  test("Multi sequence path") {
    assert(PropertyPathParser("a/b/c/d", expander) == SequencePath(
      List(
        PredicatePath(property = "prefix::a"),
        PredicatePath(property = "prefix::b"),
        PredicatePath(property = "prefix::c"),
        PredicatePath(property = "prefix::d")
      )
    ))
  }

  test("Multi alternate path") {
    assert(PropertyPathParser("a|b|c|d", expander) == AlternatePath(
      List(
        PredicatePath(property = "prefix::a"),
        PredicatePath(property = "prefix::b"),
        PredicatePath(property = "prefix::c"),
        PredicatePath(property = "prefix::d")
      )
    ))
  }

  test("Mixed path 1") {
    assert(PropertyPathParser("a/b|c/d|e", expander) == AlternatePath(
      List(
        SequencePath(
          List(
            PredicatePath(property = "prefix::a"),
            PredicatePath(property = "prefix::b")
          )
        ),
        SequencePath(
          List(
            PredicatePath(property = "prefix::c"),
            PredicatePath(property = "prefix::d")
          )
        ),
        PredicatePath(property = "prefix::e")
      )
    ))
  }

  test("Mixed path 2") {
    assert(PropertyPathParser("a/b|c|d/e/f", expander) == AlternatePath(
      List(
        SequencePath(
          List(
            PredicatePath(property = "prefix::a"),
            PredicatePath(property = "prefix::b")
          )
        ),
        PredicatePath(property = "prefix::c"),
        SequencePath(
          elements = List(
            PredicatePath(property = "prefix::d"),
            PredicatePath(property = "prefix::e"),
            PredicatePath(property = "prefix::f")
          )
        )
      )
    ))
  }

  test("Mixed path 3") {
    assert(PropertyPathParser("a|b/c/d", expander) == AlternatePath(
      List(
        PredicatePath(property = "prefix::a"),
        SequencePath(
          List(
            PredicatePath(property = "prefix::b"),
            PredicatePath(property = "prefix::c"),
            PredicatePath(property = "prefix::d")
          )
        )
      )
    ))
  }

  test("Groups path 1") {
    assert(PropertyPathParser("a/(b|c/d)|e", expander) == AlternatePath(
      List(
        SequencePath(
          List(
            PredicatePath(property = "prefix::a"),
            AlternatePath(
              List(
                PredicatePath(property = "prefix::b"),
                SequencePath(
                  List(
                    PredicatePath(property = "prefix::c"),
                    PredicatePath(property = "prefix::d")
                  )
                ),
              )
            )
          )
        ),
        PredicatePath(property = "prefix::e")
      )
    ))
  }

  test("Groups path 2") {
    assert(PropertyPathParser("a/(b|c)/((d|e)/(f/g))", expander) ==
      SequencePath(
        List(
          PredicatePath("prefix::a"),
          AlternatePath(
            List(
              PredicatePath("prefix::b"),
              PredicatePath("prefix::c")
            )
          ),
          SequencePath(
            List(
              AlternatePath(
                List(
                  PredicatePath("prefix::d"),
                  PredicatePath("prefix::e")
                )
              ),
              SequencePath(
                List(
                  PredicatePath("prefix::f"),
                  PredicatePath("prefix::g")
                )
              )
            )
          )
        )
      )
    )
  }
  test("Groups path 3") {
    assert(PropertyPathParser("a/(b|c)/((d|e)|(f/g))", expander) ==
      SequencePath(
        List(
          PredicatePath("prefix::a"),
          AlternatePath(
            List(
              PredicatePath("prefix::b"),
              PredicatePath("prefix::c")
            )
          ),
          AlternatePath(
            List(
              AlternatePath(
                List(
                  PredicatePath("prefix::d"),
                  PredicatePath("prefix::e")
                )
              ),
              SequencePath(
                List(
                  PredicatePath("prefix::f"),
                  PredicatePath("prefix::g")
                )
              )
            )
          )
        )
      )
    )
  }

  test("Simple group") {
    assert(PropertyPathParser("(((a)))", expander) == PredicatePath(property = "prefix::a"))
  }

  test("path group") {
    assert(PropertyPathParser("(((a/b)))", expander) == SequencePath(List(PredicatePath(property = "prefix::a"), PredicatePath(property = "prefix::b"))))
  }

  test("alternate group") {
    assert(PropertyPathParser("(((a|b)))", expander) == AlternatePath(List(PredicatePath(property = "prefix::a"), PredicatePath(property = "prefix::b"))))
  }

}
