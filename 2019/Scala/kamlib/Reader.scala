package kamlib

import scala.io.Source

/** File IO helper */
object Reader {

  /** Read file with the name specified in parameter */
  def readList(filename: String): List[String] =
  {
    val src = Source.fromFile(getClass.getResource("/input1.txt").getFile)
    val list = src.getLines().toList
    src.close()
    list
  }

  /** Read file with the name specified in parameter */
  def readArray(filename: String): Array[String] =
  {
    val src = Source.fromFile(getClass.getResource("/input1.txt").getFile)
    val array = src.getLines().toArray
    src.close()
    array
  }

  /** Read file with the name specified in parameter */
  def readString(filename: String): String =
  {
    val src = Source.fromFile(getClass.getResource("/input1.txt").getFile)
    val string = src.getLines().mkString
    src.close()
    string
  }

}
