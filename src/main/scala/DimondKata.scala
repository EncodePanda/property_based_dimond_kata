object DimondKata {

  type Line = List[String]

  def dimond(c: Char): List[Line] = (1 to numberOfLines(c)).toList.map(line => List.empty[String])

  private   def numberOfLines(c: Char) = (2 * (c.toInt - 'A'.toInt) + 1)

}
