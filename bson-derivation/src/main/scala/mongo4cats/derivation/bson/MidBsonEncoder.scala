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
import org.bson.{BsonDocument, BsonInt32, BsonInt64, BsonNull, BsonString}

import java.time.Instant

trait MidBsonEncoder {

  implicit val stringBsonEncoder: BsonEncoder[String] = BsonEncoder.instance(new BsonString(_))
  implicit val byteBsonEncoder: BsonEncoder[Byte]     = BsonEncoder.instance(byte => new BsonInt32(byte.toInt))
  implicit val shortBsonEncoder: BsonEncoder[Short]   = BsonEncoder.instance(short => new BsonInt32(short.toInt))
  implicit val intBsonEncoder: BsonEncoder[Int]       = BsonEncoder.instance(new BsonInt32(_))
  implicit val longBsonEncoder: BsonEncoder[Long]     = BsonEncoder.instance(new BsonInt64(_))

  implicit val instantBsonEncoder: BsonEncoder[Instant] =
    BsonEncoder.instance(instant => new BsonDocument("$date", new BsonString(instant.toString)))

  implicit val encodeObjectId: BsonEncoder[org.bson.types.ObjectId] = {
    val dollarOid: String = s"$$oid"
    BsonEncoder.instance(oid => new BsonDocument(dollarOid, new BsonString(oid.toHexString)))
  }

}
