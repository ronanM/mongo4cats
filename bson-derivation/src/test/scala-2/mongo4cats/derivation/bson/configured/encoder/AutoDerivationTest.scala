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
import io.circe.generic.extras.auto._
import io.circe.{Encoder, Json, ParsingFailure}
import mongo4cats.circe._
import mongo4cats.derivation.bson.AllBsonEncoders._
import mongo4cats.derivation.bson.AllBsonDecoders._
import mongo4cats.derivation.bson.configured.decoder.auto._
import mongo4cats.derivation.bson.configured.encoder.auto._
import mongo4cats.derivation.bson.{BsonDecoder, BsonEncoder, BsonValueOps}
import org.bson.BsonDocument
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck._
import org.scalacheck.cats.implicits._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

final case class RootTestData(
    rootTuple2Opt: Option[(String, Int)],
    rootTuple2OptWithDefault: Option[(String, Int)] = ("toto", 10).some,
    int: Option[Int] = 42.some,
    testSealedTrait: TestSealedTrait,
    items: List[ItemTestDatas],
    rootTuple2: (String, Int)
)

final case class ItemTestDatas(
    listItems: List[ItemTestData],
    setItems: Set[ItemTestData],
    optionItem: Option[ItemTestData]
)

final case class ItemTestData(
    idData: TestData[_root_.cats.Id],
    listData: TestData[List],
    setData: TestData[Set],
    optionData: TestData[Option]
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
  final case object CObj1 extends TestSealedTrait
  final case object CObj2 extends TestSealedTrait
  final case class CC1(
      objId: org.bson.types.ObjectId,
      string: String,
      instant: java.time.Instant
  ) extends TestSealedTrait
  final case class CC2(
      int: Option[Int] = 44.some,
      long: Long
  ) extends TestSealedTrait
  final case class CC3(
      byte: Byte,
      short: Short,
      tuple2: (Long, Int),
      tuple2Opt: Option[(String, Int)],
      tuple2OptWithDefault: Option[(String, Int)] = ("ten", 42).some
  ) extends TestSealedTrait
}

class AutoDerivationTest extends AnyWordSpec with ScalaCheckDrivenPropertyChecks {

  implicit val objectIdArb: Arbitrary[org.bson.types.ObjectId] =
    Arbitrary((Gen.choose(0, 16777215), Gen.choose(0, 16777215)).mapN(new org.bson.types.ObjectId(_, _)))

  implicit val shortStringArb: Arbitrary[String] =
    Arbitrary(Gen.choose(0, 5).flatMap(Gen.stringOfN(_, Gen.alphaChar)))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  "Derived direct Encode/Decode ADT to org.bson.BsonValue with same result as using mongo4cats.circe with io.circe.generic.extras.auto" in {
    val PRINTLN = false

    val prefix = "\n\n\n"

    val transformationGen: Gen[String => String] = Gen.oneOf(
      Seq(
        identity[String](_),
        // (_: String).toUpperCase,
        mongo4cats.derivation.bson.configured.Configuration.kebabCaseTransformation,
        mongo4cats.derivation.bson.configured.Configuration.snakeCaseTransformation
      )
    )

    val bsonConfigurationGen: Gen[mongo4cats.derivation.bson.configured.Configuration] =
      (
        transformationGen,
        transformationGen,
        Gen.oneOf(false, true)
        // Gen.option(Gen.stringOfN(5, Gen.alphaLowerChar))
      ).mapN(mongo4cats.derivation.bson.configured.Configuration(_, _, _, none))

    forAll(Arbitrary.arbitrary[RootTestData], bsonConfigurationGen, Gen.oneOf(false, true)) {
      case (testData, bsonConfiguration, dropNulls) =>
        // println(bsonConfigurationGen)

        implicit val bsonConf: mongo4cats.derivation.bson.configured.Configuration = bsonConfiguration

        val bsonEncoder: BsonEncoder[RootTestData] = BsonEncoder[RootTestData]
        val bsonDecoder: BsonDecoder[RootTestData] = BsonDecoder[RootTestData]

        implicit val circeConf: io.circe.generic.extras.Configuration =
          io.circe.generic.extras.Configuration(
            transformMemberNames = bsonConf.transformMemberNames,
            transformConstructorNames = bsonConf.transformConstructorNames,
            useDefaults = bsonConf.useDefaults,
            discriminator = bsonConf.discriminator
          )

        val jsonEncoder: Encoder[RootTestData] = Encoder[RootTestData]

        // --- Encode ---
        // if (PRINTLN) println(s"\n---\nScala: $p")
        if (PRINTLN) println("\n" * 10 + "-----------------------------------")
        val circeJson: Json      = jsonEncoder(testData)
        val circeJsonStr: String = circeJson.noSpaces
        if (PRINTLN) println(s"${prefix}With Circe: $circeJsonStr")

        val bsonDoc: BsonDocument = bsonEncoder(testData).asInstanceOf[BsonDocument]
        if (PRINTLN) println(s"${prefix}With Bson    : $bsonDoc")

        val bsonStr: String = bsonDoc.toJson().replace("\": ", "\":").replace(", ", ",")
        if (PRINTLN) println(s"${prefix}Bson Str: $bsonStr\n---\nJson Str: $circeJsonStr")
        assert(
          bsonStr == circeJsonStr,
          s"""|, 10) Json String from Bson != Json String from Circe
            |Bson: $bsonStr
            |Json: $circeJsonStr""".stripMargin
        )

        // --- Decode ---
        val jsonFromBsonStrEither: Either[ParsingFailure, Json] = io.circe.parser.parse(bsonStr)
        if (PRINTLN) println(s"${prefix}Bson -> Json, then parsed with Circe: ${jsonFromBsonStrEither.map(_.noSpaces)}")
        if (PRINTLN) println(s"${prefix}Circe Json                          : ${circeJsonStr.asRight}")

        assert(jsonFromBsonStrEither.isRight, ", 20) Can't be JsonDecoded")
        assert(jsonFromBsonStrEither == circeJson.asRight, ", 30) Json Decoded != Circe Json Encoded")

        val decodedFromBsonEither = bsonDecoder(if (dropNulls) bsonDoc.deepDropNullValues else bsonDoc)
        val expected              = (if (circeConf.useDefaults || dropNulls) circeJson.deepDropNullValues else circeJson).as[RootTestData]

        if (PRINTLN) println(s"${prefix}    Bson Decoded: $decodedFromBsonEither")
        if (PRINTLN) println(s"${prefix}Expected Decoded: $expected")

        assert(decodedFromBsonEither == expected, ", 40) BsonDecoder != Circe Decoder")
    }
  }
}
