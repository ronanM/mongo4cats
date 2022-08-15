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

import mongo4cats.AsJava
import org.bson.{BsonArray, BsonValue}

/** A type class that provides a conversion from a value of type `A` to a [[BsonValue]] value. */
trait BsonEncoder[A] extends Serializable with AsJava { self =>

  /** Convert a value to JSON. */
  def apply(a: A): BsonValue

  /** Create a new [[BsonEncoder]] by applying a function to a value of type `B` before encoding as an `A`. */
  final def contramap[B](f: B => A): BsonEncoder[B] = new BsonEncoder[B] {
    final def apply(a: B): BsonValue = self(f(a))
  }

  /** Create a new [[BsonEncoder]] by applying a function to the output of this one.
    */
  final def mapJson(f: BsonValue => BsonValue): BsonEncoder[A] = new BsonEncoder[A] {
    final def apply(a: A): BsonValue = f(self(a))
  }
}

object BsonEncoder extends ScalaVersionDependentEncoder with MidBsonEncoder with AsJava {

  def apply[A](implicit ev: BsonEncoder[A]): BsonEncoder[A] = ev

  def instance[A](f: A => BsonValue): BsonEncoder[A] = (a: A) => f(a)

  implicit final def encodeOption[A](implicit encA: BsonEncoder[A]): BsonEncoder[Option[A]] =
    instance {
      case Some(v) => encA(v)
      case None    => org.bson.BsonNull.VALUE
    }

  implicit final def encodeSeq[L[_] <: Seq[_], A](implicit encA: BsonEncoder[A]): BsonEncoder[L[A]] =
    instance {
      case asSeq: Seq[A] @unchecked =>
        val arrayList = new java.util.ArrayList[BsonValue](asSeq.size)
        asSeq.foreach(a => arrayList.add(encA(a)))
        new BsonArray(arrayList)
      case _ => throw new Throwable("Not a Seq")
    }

  implicit final def tuple2BsonEncoder[A, B](implicit encA: BsonEncoder[A], encB: BsonEncoder[B]): BsonEncoder[(A, B)] =
    instance { case (a, b) => new BsonArray(java.util.List.of(encA(a), encB(b))) }
}
