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

package mongo4cats.derivation.bson

import cats.syntax.all._
import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonInt64, BsonNull, BsonString, BsonValue}

import scala.jdk.CollectionConverters._
import java.time.Instant
import scala.collection.Factory

/** A type class that provides a way to produce a value of type `A` from a [[org.bson.BsonValue]] value. */
trait BsonDecoder[A] {

  /** Decode the given [[org.bson.BsonValue]]. */
  def apply(bson: BsonValue): BsonDecoder.Result[A]

}

object BsonDecoder {

  type Result[A] = Either[Throwable, A]

  def apply[A](implicit ev: BsonDecoder[A]): BsonDecoder[A] = ev

  def instance[A](f: BsonValue => Either[Throwable, A]): BsonDecoder[A] = f(_)

  implicit val byteBsonDecoder: BsonDecoder[Byte] =
    instance {
      case i: BsonInt32 => i.intValue().toByte.asRight
      case other        => new Throwable(s"Not a Byte: ${other}").asLeft
    }

  implicit val shortBsonDecoder: BsonDecoder[Short] =
    instance {
      case i: BsonInt32 => i.intValue().toShort.asRight
      case other        => new Throwable(s"Not a Short: ${other}").asLeft
    }

  implicit val intBsonDecoder: BsonDecoder[Int] =
    instance {
      case i: BsonInt32 => i.intValue().asRight
      case other        => new Throwable(s"Not an Int: ${other}").asLeft
    }

  implicit val longBsonDecoder: BsonDecoder[Long] =
    instance {
      case i: BsonInt64 => i.longValue().asRight
      case other        => new Throwable(s"Not a Long: ${other}").asLeft
    }

  implicit val stringBsonDecoder: BsonDecoder[String] =
    instance {
      case s: BsonString => s.getValue.asRight
      case other         => new Throwable(s"Not a String: ${other}").asLeft
    }

  implicit val instantBsonDecoder: BsonDecoder[Instant] =
    instance {
      case s: BsonDocument =>
        s.get("$date") match {
          case s: BsonString => Instant.parse(s.getValue).asRight
          case other         => new Throwable(s"Not a Instant: ${other}").asLeft
        }
      // s.getValue.asRight.map(Instant.ofEpochMilli)
      case other => new Throwable(s"Not a Instant: ${other}").asLeft
    }

  implicit def iterableBsonDecoder[L[_], A](implicit decA: BsonDecoder[A], factory: Factory[A, L[A]]): BsonDecoder[L[A]] =
    instance {
      case vs: BsonArray => vs.getValues.asScala.toList.traverse(decA(_)).map(_.to(factory))
      case other         => new Throwable(s"Not a Iterable: ${other}").asLeft
    }

  implicit def optionBsonDecoder[A](implicit decA: BsonDecoder[A]): BsonDecoder[Option[A]] =
    instance {
      case _: BsonNull => none.asRight
      case bsonValue   => decA(bsonValue).map(_.some)
    }
}
