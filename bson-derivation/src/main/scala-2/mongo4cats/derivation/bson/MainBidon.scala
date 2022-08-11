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
import io.circe._
import io.circe.generic.auto._

final case class Person(
    name: String,
    age: Option[Int]
)

object MainBidon extends MidBsonEncoder {

  def main(args: Array[String]): Unit = {
    import mongo4cats.derivation.bson.derivation.encoder.auto._

    implicitly[BsonEncoder[String]]
    implicitly[BsonEncoder[Int]]

    println(BsonEncoder[Person].apply(Person("toto", 10.some)))
    println(BsonEncoder[Person].apply(Person("toto", none)))
    println(Encoder[Person].apply(Person("toto", 10.some)))
    println(Encoder[Person].apply(Person("toto", none)))
  }
}
