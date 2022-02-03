object ReverseInParenthesis extends App {

  def reverseInParentheses(inputString: String): String = {
    val InnerExpression = "([a-z]+)\\(([a-z]+)\\)([a-z]+)".r
    val LeftExpression = "\\(([a-z]+)\\)([a-z]+)".r
    val RightExpression = "([a-z]+)\\(([a-z]+)\\)".r
    val LookAheadExpression = "/\\(([^)]+)\\)/".r
    val result = inputString match {
      case LookAheadExpression(content) => s"Found content $content"
      case InnerExpression(prefix,content,suffix) => s"Prefix is $prefix and content is $content and suffix $suffix"
      case LeftExpression(content, suffix) => s"Content is $content and suffix is $suffix"
      case RightExpression(prefix, content) => s"Prefix is $prefix and content is $content"
      case _ => "No match"
    }
    result
  }
  val basicInnerString = "foo(bar)blim"
  val basicLeftString = "(foo)barblim"
  val basicRightString = "foobar(blim)"
  val inputString = "foo(bar(baz))blim"
  println(reverseInParentheses(basicInnerString))
}
