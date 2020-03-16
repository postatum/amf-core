package amf.core.validation.model

abstract sealed class PropertyPath

case class PredicatePath(property: String = "", inverse: Boolean = false, zeroOrMore: Boolean = false) extends PropertyPath
case class SequencePath(elements: List[PropertyPath] = Nil) extends PropertyPath
case class AlternatePath(elements: List[PropertyPath] = Nil) extends PropertyPath
case class OpenGroup() extends PropertyPath

/**
 * Parser for SPARQL path expressions, to be used when processing properties in custom validation dialects
 */
object PropertyPathParser {

  def apply(path: String, expander: String => String): PropertyPath = {
    if (path.startsWith("file://") || path.startsWith("http://") || path.startsWith("https://")) {
      PredicatePath(expander(path))
    } else if (path.contains("/") || path.contains("|") || path.contains("^") || path.contains("*") || path.contains("(") || path.contains(")")) {
      parse(path, expander)
    } else {
      PredicatePath(expander(path))
    }
  }

  protected def parse(source: String, expander: String => String): PropertyPath = {
    val path = source.replace(" ", "")
    var current: PredicatePath = PredicatePath()
    var empty = true
    var stack: List[PropertyPath] = Nil

    for {
      (c, i) <- path.zipWithIndex
    } yield {
      // we check if the token is empty
      if (current.property == "") { empty = true } else { empty = false }
      val currentList = if (empty) { Nil } else { List(current) }

      c match {
        case '/' =>
          stack match {
            case (s:SequencePath) :: rest                         =>
              stack = (s.copy(elements = s.elements ++ expand(currentList, expander)) :: rest)
            case  _ if !empty                                     => // first position in the seq
              stack = SequencePath(elements = expand(currentList, expander)) :: stack
            case  e :: rest if empty                              => // first position in the seq
              stack = SequencePath(elements = List(e) ++ expand(currentList, expander)) :: rest
            case _  =>
              throw new Exception(s"Syntax error leading / at path or group, index: $i")
          }
          current = PredicatePath()
        case '|' =>
          stack match {
            case (s: SequencePath) :: rest  =>
              stack = SequencePath() :: (s.copy(elements = s.elements ++ expand(currentList, expander))) :: rest
            case _  if !empty               =>
              stack = (SequencePath() :: AlternatePath(expand(currentList, expander)) :: stack)
            case  e :: rest if empty        => // first position in the seq
              stack = AlternatePath(elements = List(e) ++ expand(currentList, expander)) :: rest
            case _                          =>
              throw new Exception(s"Unexpected alternate state in stack at $c in position $i")
          }
          current = PredicatePath()
        case '^' =>
          current = current.copy(inverse =  true)
        case '*' =>
          current = current.copy(zeroOrMore =  true)
        case '(' =>
          stack match {
            case (s: SequencePath) :: rest   =>
              stack = OpenGroup() :: (s.copy(elements = s.elements ++ expand(currentList, expander)) :: rest)
            case (a: AlternatePath) :: rest  =>
              stack = OpenGroup() :: (a.copy(elements = a.elements ++ expand(currentList, expander)) :: rest)
            case  _                          =>
              stack = OpenGroup() :: stack
          }
          current = PredicatePath()
        case ')' =>
          stack match {
            case (s:SequencePath) :: rest  =>
              stack = (s.copy(elements = s.elements ++ expand(currentList, expander)) :: rest)
            case (a:AlternatePath) :: rest =>
              stack = (a.copy(elements = a.elements ++ expand(currentList, expander)) :: rest)
            case (og: OpenGroup) :: rest   =>
              stack = SequencePath(expand(currentList, expander)) :: og :: rest
            case t                         =>
              throw new Exception(s"Property path parsing error '${path}' at position $i ')', unknown token in stack: ${t}")
          }
          current = PredicatePath()
          val group = stack.takeWhile( e => !e.isInstanceOf[OpenGroup])
          val rest = stack.dropWhile(e => !e.isInstanceOf[OpenGroup]).drop(1)
          val nextElement = reduce(group)
          // generate the new stack
          stack = rest match {
            case (es: SequencePath) :: rest   => es.copy(es.elements ++ List(nextElement)) :: rest
            case other                        => nextElement :: other
          }
        case _   =>
          current = current.copy(property = current.property + c)
      }
    }

    // final reduction

    // we check if the token is empty
    if (current.property == "") { empty = true } else { empty = false }
    val currentList = if (empty) { Nil } else { List(current) }

    val result = stack match {
      case (s:SequencePath) :: rest  =>
        reduce(s.copy(elements = s.elements ++ expand(currentList, expander)) :: rest)
      case (a:AlternatePath) :: rest =>
        reduce(a.copy(elements = a.elements ++ expand(currentList, expander)) :: rest)
      case (og: OpenGroup) :: rest   =>
        reduce(SequencePath(expand(currentList, expander)) :: og :: rest)
      case Nil                       =>
        expand(currentList, expander).head
      case _                         =>
        throw new Exception(s"Property path parsing error unexpected reduction in group found invalid stack $stack")
    }

    // simplify case for a single element in a path
    result match {
      case sp: SequencePath if sp.elements.size == 1 => sp.elements.head
      case _                                         => result
    }
  }


  protected def reverse(e: PropertyPath): PropertyPath = e match {
    case s: SequencePath  => s.copy(elements = s.elements.reverse.map(reverse))
    case a: AlternatePath => a.copy(elements = a.elements.reverse.map(reverse))
    case other            => other
  }

  protected def reduce(stack: List[PropertyPath]): PropertyPath = {
    stack match {
      case (et: PropertyPath) :: Nil =>
        et
      case _: List[PropertyPath]       =>
        AlternatePath(flat(stack))
      case _                          =>
        throw new Exception(s"""Unknown reduction state processing property path $stack""")
    }
  }

  def flat(es: List[PropertyPath]): List[PropertyPath] = {
    val m = es map {
      case (seq: SequencePath) if seq.elements.size == 1   => seq.elements.head
      case (alt:  AlternatePath) if alt.elements.size == 1 => alt.elements.head
      case other                                           => other
    }
    m.reverse
  }

  protected def expand(p: List[PredicatePath], expander: String => String): List[PredicatePath] = {
    p match {
      case Nil => Nil
      case e :: rest => e.copy(property = expander(e.property)) :: rest
    }
  }
}
