package com.twitter.scalding

/**
 * Created by exie on 4/10/15.
 */
object StringUtility {
  // Scala's split function uses regex which is too slow thus we create this faster function

  /* This function decides whether at certain index in a string, it's trailling with a key
   * for instance, "ab:::" is trailing at index 2 3 4.
   * This is to accomondate Java split's special treatment that if a string is trailing for the separator, then
   * it will simply trim those trailing characters.
   *
   * e.g. "ab:::".split(":") = List("a", "b") instead of List("a", "b", "")
   *
   * We don't want to check if it's trailling at each index in fastSplitHelper since doing so would make the split
   * time complexity to become O(N^2) where N is the length of the string to split
   *
   */
  private def isTrailingWith(text: String, separator: String) = {
    val result = text.foldRight((List[Boolean](), true)) {
      case (c, (tillNow, isTrailingBefore)) =>
        val trailing = isTrailingBefore && (c.toString == separator)
        (tillNow ++ List(trailing), trailing)
    }
    result._1.reverse
  }

  private def fastSplitHelper(text: String, key: String, isTrailingAt: List[Boolean]): Seq[String] = {
    if (text.isEmpty) {
      Seq()
    } else {
      val firstIndex = text.indexOf(key)
      if (firstIndex == -1) {
        Seq(text)
      } else {
        // the text till the separator should be kept in any case
        val currentChunk = text.substring(0, firstIndex)

        // if it's trailing from this index, then ignore the rest of the string
        if (isTrailingAt(firstIndex)) {
          Seq(currentChunk)
        } else {
          // Otherwise, we want to see the rest of the string
          val nextText = text.substring(firstIndex + 1)
          Seq(currentChunk) ++ fastSplitHelper(nextText, key, isTrailingAt.slice(firstIndex + 1, text.length))
        }
      }
    }
  }
  def fastSplit(text: String, key: String): Seq[String] = {
    fastSplitHelper(text, key, isTrailingWith(text, key))
  }

}
