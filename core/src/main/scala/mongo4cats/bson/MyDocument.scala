/*
 * Copyright 2020 Kirill5k
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mongo4cats.bson

import mongo4cats.codecs.{MongoCodecProvider, MyDocumentCodecProvider}
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentWrapper}
import org.bson.codecs.configuration.{CodecProvider, CodecRegistry}
import org.bson.conversions.Bson
import org.bson.json.{JsonMode, JsonReader, JsonWriter, JsonWriterSettings}

import java.io.StringWriter
import scala.annotation.tailrec

final class MyDocument private (
    val fields: Map[String, Any]
) extends Bson {

  def merge(other: MyDocument): MyDocument = new MyDocument(fields ++ other.fields)

  def isEmpty: Boolean                              = fields.isEmpty
  def contains(key: String): Boolean                = fields.contains(key)
  def keys: Set[String]                             = fields.keySet
  def remove(key: String): MyDocument               = new MyDocument(fields - key)
  def add[A](keyValuePair: (String, A)): MyDocument = new MyDocument(fields + keyValuePair)
  def add[A](key: String, value: A): MyDocument     = add(key -> value)

  def get[A](key: String): Option[A] = fields.get(key).flatMap(Option(_)).map(_.asInstanceOf[A])

  def getString(key: String): Option[String]       = get[String](key)
  def getLong(key: String): Option[Long]           = get[Long](key)
  def getDouble(key: String): Option[Double]       = get[Double](key)
  def getBoolean(key: String): Option[Boolean]     = get[Boolean](key)
  def getObjectId(key: String): Option[ObjectId]   = get[ObjectId](key)
  def getDocument(key: String): Option[MyDocument] = get[MyDocument](key)
  def getList[A](key: String): Option[List[A]]     = get[List[A]](key)

  def getNested[A](jsonPath: String): Option[A] = {
    @tailrec
    def go(currentPath: String, remainingPaths: Array[String], nestedDoc: MyDocument): Option[A] =
      if (remainingPaths.isEmpty) nestedDoc.get[A](currentPath)
      else
        nestedDoc.get[Any](currentPath) match {
          case Some(anotherDoc: MyDocument) => go(remainingPaths.head, remainingPaths.tail, anotherDoc)
          case _                            => None
        }

    val paths = jsonPath.split("\\.")
    go(paths.head, paths.tail, this)
  }

  def toJson: String = {
    val writerSettings = JsonWriterSettings.builder.outputMode(JsonMode.RELAXED).build
    val writer         = new JsonWriter(new StringWriter(), writerSettings)
    MyDocumentCodecProvider.DefaultCodec.encode(writer, this, EncoderContext.builder.build)
    writer.getWriter.toString
  }

  override def toBsonDocument[TDocument](documentClass: Class[TDocument], codecRegistry: CodecRegistry): BsonDocument =
    new BsonDocumentWrapper[MyDocument](this, codecRegistry.get(classOf[MyDocument]))

  override def toString: String = fields.toString().replaceAll("Map", "Document")
  override def hashCode(): Int  = fields.hashCode()
  override def equals(other: Any): Boolean =
    Option(other) match {
      case Some(doc: MyDocument) => doc.fields.equals(fields)
      case _                     => false
    }
}

object MyDocument {
  val empty: MyDocument = apply()

  def apply(): MyDocument                                                  = new MyDocument(Map.empty)
  def apply[A](keyValue: (String, A), keyValues: (String, A)*): MyDocument = new MyDocument(Map(keyValue) ++ keyValues.toMap)
  def apply[A](fields: Map[String, A]): MyDocument                         = new MyDocument(fields)

  def parse(json: String): MyDocument = MyDocumentCodecProvider.DefaultCodec.decode(new JsonReader(json), DecoderContext.builder().build())

  implicit val codecProvider: MongoCodecProvider[MyDocument] = new MongoCodecProvider[MyDocument] {
    override def get: CodecProvider = MyDocumentCodecProvider
  }
}
