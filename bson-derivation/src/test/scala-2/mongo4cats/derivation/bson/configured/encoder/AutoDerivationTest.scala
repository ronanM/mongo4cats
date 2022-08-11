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

package mongo4cats.derivation.bson.configured.encoder

import cats.syntax.all._
import cats.implicits.catsSyntaxEitherId
import io.circe.{Decoder, Encoder, Json, ParsingFailure}
import org.scalacheck._
import org.scalatest.wordspec.AnyWordSpec
import org.scalacheck.cats.implicits._
import io.circe.syntax._
import mongo4cats.derivation.bson.{BsonDecoder, BsonEncoder}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.ScalacheckShapeless._
import mongo4cats.derivation.bson
import mongo4cats.circe._
import org.bson.BsonValue
import io.circe.generic.extras.auto._
import mongo4cats.derivation.bson
import mongo4cats.derivation.bson.configured.encoder.auto._
import mongo4cats.derivation.bson.configured.decoder.auto._

// TODO Rajouter un `sealed trait`.
// TODO PBT sur la `io.circe.generic.extras.Configuration` / `mongo4cats.derivation.bson.configured.Configuration`.

final case class RootTestData(
    listItems: List[ItemTestData],
    setItems: Set[ItemTestData],
    optionItem: Option[ItemTestData],
    // mapIntItems: Map[Int, ItemTestData],
    int: Option[Int] = 42.some
)

final case class ItemTestData(
    idData: TestData[_root_.cats.Id],
    listData: TestData[List],
    setData: TestData[Set],
    optionData: TestData[Option],
    sealedTrait: TestSealedTrait
)

final case class TestData[L[_]](
    stringL: L[String],
    byteL: L[Byte],
    shortL: L[Short],
    intL: L[Int],
    longL: L[Long],
    instantL: L[java.time.Instant]
)

sealed trait TestSealedTrait

object TestSealedTrait {
  final case class CC1(string: String, instant: java.time.Instant) extends TestSealedTrait
  final case class CC2(int: Int, long: Long)                       extends TestSealedTrait
  final case class CC3(
      byte: Byte,
      short: Short
      // longInt: (Long, Int)
      // strOpt: Option[(String, Int)]
  ) extends TestSealedTrait
}

// $ sbt ++2.13.8 "~mongo4cats-bson-derivation/test"
class AutoDerivationTest extends AnyWordSpec with ScalaCheckDrivenPropertyChecks {

  // implicitly[Arbitrary[String]]
  // implicitly[Arbitrary[CC1]]
  // implicitly[Arbitrary[CC2]]
  // implicitly[Arbitrary[TestSealedTrait]]
  // implicitly[Arbitrary[List[Int]]]
  // implicitly[Arbitrary[List[TestSealedTrait]]]

  implicit def notTooBigIterable[L[_], A](implicit arbA: Arbitrary[L[A]]): Arbitrary[L[A]] =
    Arbitrary(Gen.choose(0, 3).flatMap(Gen.resize(_, arbA.arbitrary)))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "Encode/Decode like with Circe" in {
    val PRINTLN = false

    val prefix = "\n\n\n"

    val transformationGen: Gen[String => String] = Gen.oneOf(
      Seq(
        identity[String](_),
        (_: String).toUpperCase,
        bson.configured.Configuration.kebabCaseTransformation,
        bson.configured.Configuration.snakeCaseTransformation
      )
    )

    val bsonConfigurationGen: Gen[bson.configured.Configuration] =
      (
        transformationGen,
        transformationGen,
        Gen.oneOf(false, true).map(_ => false),
        Gen.option(Gen.stringOfN(5, Gen.alphaLowerChar)).map(_ => none)
      ).mapN(bson.configured.Configuration(_, _, _, _))

    forAll(Arbitrary.arbitrary[RootTestData], bsonConfigurationGen) { case (testData, bsonConfiguration) =>
      // println(bsonConfigurationGen)

      implicit val bsonConf: mongo4cats.derivation.bson.configured.Configuration = bsonConfiguration

      implicit val circeConf = io.circe.generic.extras.Configuration(
        transformMemberNames = bsonConf.transformMemberNames,
        transformConstructorNames = bsonConf.transformConstructorNames,
        useDefaults = bsonConf.useDefaults,
        discriminator = bsonConf.discriminator
      )

      // implicit val RootTestDataEnc = io.circe.generic.extras.semiauto.deriveConfiguredEncoder[RootTestData]
      // implicit val RootTestDataDec = io.circe.generic.extras.semiauto.deriveConfiguredDecoder[RootTestData]
      //
      // implicit val ItemTestDataEnc = io.circe.generic.extras.semiauto.deriveConfiguredEncoder[ItemTestData]
      // implicit val ItemTestDataDec = io.circe.generic.extras.semiauto.deriveConfiguredDecoder[ItemTestData]
      //
      // implicit def TestDataEnc[L[_], A](implicit la: Encoder[L[A]]) = generic.extras.semiauto.deriveConfiguredEncoder[TestData[L]]
      // implicit def TestDataDec[L[_], A](implicit la: Decoder[L[A]]) = generic.extras.semiauto.deriveConfiguredDecoder[TestData[L]]

      // import io.circe.generic.auto._
      // import io.circe.generic.extras.auto._

      // implicitly[Decoder[Option[Int]]]
      // implicitly[Encoder[Option[Int]]]

      // implicitly[BsonEncoder[Option[Int]]]
      // implicitly[BsonDecoder[Option[Int]]]

      val testDataBsonEnc = BsonEncoder[RootTestData]

      // if (PRINTLN) println(s"\n---\nScala: $p")
      if (PRINTLN) println("\n" * 10 + "-----------------------------------")
      val circeJson: Json      = testData.asJson
      val circeJsonStr: String = circeJson.noSpaces
      if (PRINTLN) println(s"${prefix}With Circe: $circeJsonStr")

      val bson: BsonValue = testDataBsonEnc(testData)
      if (PRINTLN) println(s"${prefix}With Bson    : $bson")

      val bsonStr: String = bson.toString
      if (PRINTLN) println(s"${prefix}Bson Str: $bsonStr")

      val jsonFromBsonStrEither: Either[ParsingFailure, Json] = io.circe.parser.parse(bsonStr)
      if (PRINTLN) println(s"${prefix}Bson -> Json, then parsed with Circe: ${jsonFromBsonStrEither.map(_.noSpaces)}")
      if (PRINTLN) println(s"${prefix}Circe Json                          : ${circeJsonStr.asRight}")

      assert(jsonFromBsonStrEither.isRight, ", Can't be JsonDecoded")
      assert(jsonFromBsonStrEither == circeJson.asRight, ", Json Decoded != Circe Json Encoded")

      val decodedFromBsonEither = BsonDecoder[RootTestData].apply(bson)
      val expected              = testData.asRight[Throwable]

      if (PRINTLN) println(s"${prefix}    Bson Decoded: $decodedFromBsonEither")
      if (PRINTLN) println(s"${prefix}Expected Decoded: $expected")

      assert(decodedFromBsonEither == expected, ", BsonDecoder != Circe Decoder")
    }
  }
}
