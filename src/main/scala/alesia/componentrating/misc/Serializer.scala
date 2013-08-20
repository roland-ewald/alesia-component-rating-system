/**
 * *****************************************************************************
 * Copyright 2012-2013 Jonathan Wienss, Michael Stein, Roland Ewald
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ****************************************************************************
 */
package alesia.componentrating.misc

import java.io.PrintWriter
import java.io.File
import com.thoughtworks.xstream.XStream
import scala.io.Source
import java.util.ArrayList
import scala.collection.mutable.ListBuffer

/**
 * Simple serialization support via XStream.
 *
 * @author Jonathan Wienss
 * @author Roland Ewald
 */
object Serializer {

  def listToFile[T](fileName: String, obj: Iterable[T]) = {
    val list = new ArrayList[T]()
    for (o <- obj)
      list.add(o)
    toFile(fileName, list)
  }

  def listFromFile[T](fileName: String): List[T] = {
    val list: ArrayList[T] = fromFile(fileName)
    val rv = ListBuffer[T]()
    for (i <- 0 until list.size())
      rv += list.get(i)
    rv.toList
  }

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
