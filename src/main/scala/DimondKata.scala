object DimondKata {

  type Line = List[String]

  def dimond(c: Char): List[Line] = (1 to numberOfLines(c)).toList.map(number => (1 to numberOfLines(c)).toList.map(_.toString))

  private   def numberOfLines(c: Char) = (2 * (c.toInt - 'A'.toInt) + 1)

}
