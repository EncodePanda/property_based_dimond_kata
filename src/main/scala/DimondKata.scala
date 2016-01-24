object DimondKata {

  type Line = List[Char]



  def dimond(c: Char): List[Line] = {
    (1 to numberOfLines(c)).toList
      .map(number => (1 to numberOfLines(c)).toList.map(n => ' '))
      .zipWithIndex
      .map {
      case (line, 0) => (line.take(numberOfLines(c)/2) ++ List('A') ++ line.drop(numberOfLines(c) / 2 + 1)  , 0)
      case other => other
    }
      .map {
      case (line, index) => line
    }
  }

  def middle(c: Char) = numberOfLines(c)/2

  private   def numberOfLines(c: Char) = (2 * (c.toInt - 'A'.toInt) + 1)



}
