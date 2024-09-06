
class Coder(words: List[String]):
  val mnemonics = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

/** Maps a letter to the digit it represents */
  private val charCode: Map[Char,Char] = 
    for
      (k,v) <- mnemonics
      ltr <- v
    yield (ltr,k)

/** Maps a word to the digit string it can represent */
/** incorrect implementation - assumed only first char of string needed to be mapped 
  private def wordCode(word: String): String =
    (for 
      (k,v) <- mnemonics
      if v.contains(word.head.toUpper)
    yield k
    ).mkString
*/
  private def wordCode(word: String): String =
    word.toUpperCase.map(charCode)

/** Maps a digit string to all words in the dictionary that represent it */
  /**
  def wordsForNum(words: List[String]): Map[String, List[String]] = 
  words.map(x => (wordCode(x),x)).groupMap((k,v) => k)(x => x._2)
  */

  private val wordsForNum: Map[String, List[String]] =
    words.groupBy(wordCode).withDefaultValue(Nil)


/** All ways to encode a number as a list of words */

    //val dictionary = List("average","avengers","you")
    //val wordsMappings = wordsForNum(dictionary)
  /** def encode(number: String): Set[List[String]] =
    (for
      n <- number
      (k,v) <- wordsMappings
      if n == k.toList.head
    yield v
    ).toSet
    */
    //722563 -> List(7,72,722....)
    def encode(number: String): Set[List[String]] =
      if number.isEmpty then Set(Nil)
      else 
        for 
          splitPoint <- (1 to number.length).toSet
          word <- wordsForNum(number.take(splitPoint))
          rest <- encode(number.drop(splitPoint))
        yield word :: rest


        
@main def code(number: String) = 
  val coder = Coder(List(
    "Scala","Python","Ruby","C",
    "rocks","socks","sucks","works","pack"))
  coder.encode(number).map(_.mkString(" "))
