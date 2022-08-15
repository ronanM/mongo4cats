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
import mongo4cats.bson.ObjectId
import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonInt64, BsonNull, BsonString, BsonValue}

import scala.jdk.CollectionConverters._
import java.time.Instant
import scala.util.Try

/** A type class that provides a way to produce a value of type `A` from a [[org.bson.BsonValue]] value. */
trait BsonDecoder[A] {

  /** Decode the given [[org.bson.BsonValue]]. */
  def apply(bson: BsonValue): BsonDecoder.Result[A]

}

object BsonDecoder extends ScalaVersionDependentDecoder {

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
      case other => new Throwable(s"Not a Instant: ${other}").asLeft
    }

  implicit val decodeObjectId: BsonDecoder[org.bson.types.ObjectId] =
    instance {
      case s: BsonDocument =>
        s.get("$oid") match {
          case s: BsonString => new org.bson.types.ObjectId(s.getValue).asRight
          case other         => new Throwable(s"Not a ObjectId: ${other}").asLeft
        }
      case other => new Throwable(s"Not a ObjectId: ${other}").asLeft
    }

  implicit def optionBsonDecoder[A](implicit decA: BsonDecoder[A]): BsonDecoder[Option[A]] =
    instance(a =>
      if (a == null || a.isNull) none.asRight
      else decA(a).map(_.some)
    )

  implicit def tuple2BsonDecoder[A, B](implicit decA: BsonDecoder[A], decB: BsonDecoder[B]): BsonDecoder[(A, B)] =
    instance {
      case arr: BsonArray if arr.size() == 2 => (decA(arr.get(0)), decB(arr.get(1))).tupled
      case arr: BsonArray                    => new Throwable(s"Not an array of size 2: ${arr}").asLeft
      case other                             => new Throwable(s"Not an array: ${other}").asLeft
    }
}
