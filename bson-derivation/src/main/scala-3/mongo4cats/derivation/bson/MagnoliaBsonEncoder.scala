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

import magnolia1._
import mongo4cats.derivation.bson.configured.Configuration
import org.bson.{BsonDocument, BsonValue}

private[bson] object MagnoliaBsonEncoder {

  private[bson] def join[T](caseClass: CaseClass[BsonEncoder, T])(implicit config: Configuration): BsonEncoder[T] = {
    val paramJsonKeyLookup =
      caseClass.params.map { p =>
        val jsonKeyAnnotation = p.annotations.collectFirst { case ann: BsonKey => ann }

        jsonKeyAnnotation match {
          case Some(ann) => p.label -> ann.value
          case None      => p.label -> config.transformMemberNames(p.label)
        }
      }.toMap

    if (paramJsonKeyLookup.values.toList.distinct.size != caseClass.params.length) {
      throw new BsonDerivationError(
        "Duplicate key detected after applying transformation function for case class parameters"
      )
    }

    new BsonEncoder[T] {
      def apply(a: T): BsonValue = {
        val bsonDoc = new BsonDocument(caseClass.params.size)

        caseClass.params
          .map { p =>
            val label = paramJsonKeyLookup.getOrElse(
              p.label,
              throw new IllegalStateException("Looking up a parameter label should always yield a value. This is a bug")
            )
            label -> p.typeclass(p.deref(a))
          }
          .foreach { case (k, v) => bsonDoc.append(k, v) }

        bsonDoc
      }
    }
  }

  private[bson] def split[T](
      sealedTrait: SealedTrait[BsonEncoder, T]
  )(implicit config: Configuration): BsonEncoder[T] = {
    {
      val origTypeNames = sealedTrait.subtypes.map(_.typeInfo.short)
      val transformed   = origTypeNames.map(config.transformConstructorNames).distinct
      if (transformed.length != origTypeNames.length) {
        throw new BsonDerivationError(
          "Duplicate key detected after applying transformation function for " +
            "sealed trait child classes"
        )
      }
    }

    new BsonEncoder[T] {
      def apply(a: T): BsonValue =
        sealedTrait.choose(a) { subtype =>
          val baseJson = subtype.typeclass(subtype.cast(a))
          val constructorName = config
            .transformConstructorNames(subtype.typeInfo.short)
          config.discriminator match {
            case Some(discriminator) =>
              // Note: Here we handle the edge case where a subtype of a sealed trait has a custom encoder which does not encode
              // encode into a JSON object and thus we cannot insert the discriminator key. In this case we fallback
              // to the non-discriminator case for this subtype. This is same as the behavior of circe-generic-extras
              baseJson match {
                case bsonDoc: BsonDocument => bsonDoc.clone().append(discriminator, org.bson.Document.parse(constructorName).toBsonDocument)
                case _                     => new BsonDocument(constructorName, baseJson)
              }
            case None => new BsonDocument(constructorName, baseJson)
          }
        }
    }
  }
}