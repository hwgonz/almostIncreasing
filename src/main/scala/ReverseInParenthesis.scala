
/*
You are given a string that consists of lower case English letters and brackets.

Reverse the strings in each pair of matching parentheses, starting from the innermost one.

Your result should not contain any brackets.

*/

object ReverseInParenthesis extends App {

  val inputString = "u(tuoutf()pa(x(ykigx(wq))(()()cj()(xicb)najyeiw)u(oxn)f(ga(jimkd)(a)rylu)ass)dge)iqk(n()t((yc(afs((a)((j)qf(ksvqg(s)sin)fy()(s(((lid)jy(p(bo()((()jrs)))zauve(krhact(lpw))ne))ng)gm(i)f)t)h()tois)mu))((au)mzpc)(ilqa(tnprm)hmul(y(er)pjj)qnqcd(exx)(pmjkd)(nhexjxhuc)(ehq)t()uo(gnbh)cpmjofgd)(oio()twrzssjt)()iln(vprgttrt(d((yhiwae)ix()owemq)(((pk()h(((jdpt()()l()jkq()mrm((gnu)mq)srz(ua(nofa(nf))dzkikdwlr)ww)qf(q)jhm)()h(o(kt)kbm(ilqr)tq)h(mo)))(g)ln)nmgj)y(()jwetph))(u)mpf(j((xujbp)ezg(((nucbr(g)ckndk))))))))"

  def reverseInParenthesis(inputString: String): String =
    hasParenthesis(inputString) match {
      case false => inputString
      case true => reverseInParenthesis(removeInnerParenthesis(inputString))
    }

  def hasParenthesis(inputString: String): Boolean =
    inputString.contains("(") && inputString.contains(")")

  def removeInnerParenthesis(inputString: String): String = {
    val cleanInputString = inputString.replaceAll("\\(\\)", "")
    val LookInsideParentheses = """(?<=\()[a-z]+(?=\))""".r
    val resultInside = LookInsideParentheses.findFirstIn(cleanInputString).mkString
    if (resultInside.isEmpty)
      cleanInputString
    else {
      val fixedString = cleanInputString.substring(0,cleanInputString.indexOf(s"($resultInside)")) + resultInside.reverse + cleanInputString.substring(cleanInputString.indexOf(s"($resultInside)") + 2 + resultInside.length)
      removeInnerParenthesis(fixedString)
    }
  }

  println(reverseInParenthesis(inputString))

}
