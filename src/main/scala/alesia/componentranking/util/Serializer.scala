

package alesia.componentranking.util

import java.io.PrintWriter
import java.io.File
import com.thoughtworks.xstream.XStream
import scala.io.Source

/**
 * Simple serialization support via XStream.
 *
 * @author Jonathan Wienss
 * @author Roland Ewald
 */
object Serializer {

  def toFile(fileName: String, obj: Any) = {
    val writer = new PrintWriter(new File(fileName))
    try {
      writer.print(new XStream().toXML(obj))
    } finally { writer.close }
  }

  def fromFile[T](fileName: String): T =
    new XStream().fromXML(Source.fromFile(fileName).mkString).asInstanceOf[T]

  def fromFileOrElse[T](fileName: String, orElse: T): T =
    try fromFile(fileName) catch { case ioe: java.lang.Exception => orElse }

}