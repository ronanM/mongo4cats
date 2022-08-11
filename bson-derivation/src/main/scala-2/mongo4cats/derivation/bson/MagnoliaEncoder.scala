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

import mongo4cats.derivation.bson.configured.Configuration
import io.circe.{Encoder, Json}
import magnolia1._

private[bson] object MagnoliaEncoder {

  private[bson] def combine[T](caseClass: CaseClass[Encoder, T])(implicit config: Configuration): Encoder[T] = {
    val paramJsonKeyLookup = caseClass.parameters.map { p =>
      val jsonKeyAnnotation = p.annotations.collectFirst { case ann: JsonKey =>
        ann
      }
      jsonKeyAnnotation match {
        case Some(ann) => p.label -> ann.value
        case None      => p.label -> config.transformMemberNames(p.label)
      }
    }.toMap

    if (paramJsonKeyLookup.values.toList.distinct.size != caseClass.parameters.length) {
      throw new DerivationError(
        "Duplicate key detected after applying transformation function for case class parameters"
      )
    }

    new Encoder[T] {
      def apply(a: T): Json =
        Json.obj(caseClass.parameters.map { p =>
          val label = paramJsonKeyLookup.getOrElse(
            p.label,
            throw new IllegalStateException("Looking up a parameter label should always yield a value. This is a bug")
          )
          label -> p.typeclass(p.dereference(a))
        }: _*)
    }
  }

  private[bson] def dispatch[T](
      sealedTrait: SealedTrait[Encoder, T]
  )(implicit config: Configuration): Encoder[T] = {
    {
      val origTypeNames = sealedTrait.subtypes.map(_.typeName.short)
      val transformed   = origTypeNames.map(config.transformConstructorNames).distinct
      if (transformed.length != origTypeNames.length) {
        throw new DerivationError(
          "Duplicate key detected after applying transformation function for " +
            "sealed trait child classes"
        )
      }
    }

    new Encoder[T] {
      def apply(a: T): Json =
        sealedTrait.split(a) { subtype =>
          val baseJson = subtype.typeclass(subtype.cast(a))
          val constructorName = config
            .transformConstructorNames(subtype.typeName.short)
          config.discriminator match {
            case Some(discriminator) =>
              // Note: Here we handle the edge case where a subtype of a sealed trait has a custom encoder which does not encode
              // encode into a JSON object and thus we cannot insert the discriminator key. In this case we fallback
              // to the non-discriminator case for this subtype. This is same as the behavior of circe-generic-extras
              baseJson.asObject match {
                case Some(jsObj) => Json.fromJsonObject(jsObj.add(discriminator, Json.fromString(constructorName)))
                case None        => Json.obj(constructorName -> baseJson)
              }
            case None =>
              Json.obj(constructorName -> baseJson)
          }
        }
    }
  }

}
