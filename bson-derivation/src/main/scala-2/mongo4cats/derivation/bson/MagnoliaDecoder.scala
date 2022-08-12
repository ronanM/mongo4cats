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

import cats.syntax.either._
import io.circe.{Decoder, DecodingFailure, HCursor}
import io.circe.Decoder.Result
import magnolia1._
import mongo4cats.derivation.bson.configured.Configuration

private[bson] object MagnoliaDecoder {

  private[bson] def join[T](
      caseClass: CaseClass[Decoder, T]
  )(implicit configuration: Configuration): Decoder[T] = {
    val paramJsonKeyLookup: Map[String, String] = caseClass.parameters.map { p =>
      val jsonKeyAnnotation = p.annotations.collectFirst { case ann: BsonKey =>
        ann
      }
      jsonKeyAnnotation match {
        case Some(ann) => p.label -> ann.value
        case None      => p.label -> configuration.transformMemberNames(p.label)
      }
    }.toMap

    if (paramJsonKeyLookup.values.toList.distinct.length != caseClass.parameters.length) {
      throw new DerivationError("Duplicate key detected after applying transformation function for case class parameters")
    }

    if (configuration.useDefaults) {
      new Decoder[T] {
        override def apply(c: HCursor): Result[T] =
          caseClass
            .constructEither { p =>
              val key = paramJsonKeyLookup.getOrElse(
                p.label,
                throw new IllegalStateException("Looking up a parameter label should always yield a value. This is a bug")
              )
              val keyCursor = c.downField(key)
              keyCursor.focus match {
                case Some(_) => p.typeclass.tryDecode(keyCursor)
                case None =>
                  p.default.fold {
                    // Some decoders (in particular, the default Option[T] decoder) do special things when a key is missing,
                    // so we give them a chance to do their thing here.
                    p.typeclass.tryDecode(keyCursor)
                  }(x => Right(x))
              }
            }
            .leftMap(_.head)
      }
    } else {
      new Decoder[T] {
        def apply(c: HCursor): Result[T] =
          caseClass
            .constructEither { p =>
              p.typeclass.tryDecode(
                c.downField(
                  paramJsonKeyLookup.getOrElse(
                    p.label,
                    throw new IllegalStateException("Looking up a parameter label should always yield a value. This is a bug")
                  )
                )
              )
            }
            .leftMap(_.head)
      }
    }
  }

  private[bson] def split[T](
      sealedTrait: SealedTrait[Decoder, T]
  )(implicit configuration: Configuration): Decoder[T] = {

    val constructorLookup: Map[String, Subtype[Decoder, T]] =
      sealedTrait.subtypes.map { s =>
        configuration.transformConstructorNames(s.typeName.short) -> s
      }.toMap

    if (constructorLookup.size != sealedTrait.subtypes.length) {
      throw new DerivationError("Duplicate key detected after applying transformation function for case class parameters")
    }

    configuration.discriminator match {
      case Some(discriminator) => new DiscriminatedDecoder[T](discriminator, constructorLookup)
      case None                => new NonDiscriminatedDecoder[T](constructorLookup)
    }
  }

  private[bson] class NonDiscriminatedDecoder[T](constructorLookup: Map[String, Subtype[Decoder, T]]) extends Decoder[T] {
    private val knownSubTypes = constructorLookup.keys.toSeq.sorted.mkString(",")

    override def apply(c: HCursor): Result[T] =
      c.keys match {
        case Some(keys) if keys.size == 1 =>
          val key = keys.head
          for {
            theSubtype <- Either.fromOption(
              constructorLookup.get(key),
              DecodingFailure(
                s"""Can't decode coproduct type: couldn't find matching subtype.
                       |JSON: ${c.value},
                       |Key: $key
                       |Known subtypes: $knownSubTypes\n""".stripMargin,
                c.history
              )
            )

            result <- c.get(key)(theSubtype.typeclass)
          } yield result
        case _ =>
          Left(
            DecodingFailure(
              s"""Can't decode coproduct type: zero or several keys were found, while coproduct type requires exactly one.
                   |JSON: ${c.value},
                   |Keys: ${c.keys.map(_.mkString(","))}
                   |Known subtypes: $knownSubTypes\n""".stripMargin,
              c.history
            )
          )
      }
  }

  private[bson] class DiscriminatedDecoder[T](discriminator: String, constructorLookup: Map[String, Subtype[Decoder, T]])
      extends Decoder[T] {
    val knownSubTypes = constructorLookup.keys.toSeq.sorted.mkString(",")

    override def apply(c: HCursor): Result[T] =
      c.downField(discriminator).as[String] match {
        case Left(_) =>
          Left(
            DecodingFailure(
              s"""
             |Can't decode coproduct type: couldn't find discriminator or is not of type String.
             |JSON: ${c.value}
             |discriminator key: discriminator
              """.stripMargin,
              c.history
            )
          )
        case Right(ctorName) =>
          constructorLookup.get(ctorName) match {
            case Some(subType) => subType.typeclass.apply(c)
            case None =>
              Left(
                DecodingFailure(
                  s"""
               |Can't decode coproduct type: constructor name not found in known constructor names
               |JSON: ${c.value}
               |Allowed discriminators: $knownSubTypes
              """.stripMargin,
                  c.history
                )
              )
          }
      }

  }
}
