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

package mongo4cats.derivation.bson.configured.decoder

import mongo4cats.derivation.bson.{BsonDecoder, MagnoliaBsonDecoder}
import mongo4cats.derivation.bson.configured.Configuration
import magnolia1.{CaseClass, Magnolia, SealedTrait}

object semiauto {

  type Typeclass[T] = BsonDecoder[T]

  def join[T](caseClass: CaseClass[Typeclass, T])(implicit configuration: Configuration): Typeclass[T] =
    MagnoliaBsonDecoder.join(caseClass)

  def split[T](sealedTrait: SealedTrait[Typeclass, T])(implicit configuration: Configuration): Typeclass[T] =
    MagnoliaBsonDecoder.split(sealedTrait)

  def magnoliaConfiguredDecoder[T]: Typeclass[T] = macro Magnolia.gen[T]
}
