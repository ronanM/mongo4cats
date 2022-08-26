package mongo4cats.bench

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Fork
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Warmup
import cats.syntax.all._
import io.circe.syntax._
import io.circe.generic.extras.auto._
import mongo4cats.bench.BenchST.BenchST2
import mongo4cats.circe._
import mongo4cats.derivation.bson.AllBsonEncoders._
import mongo4cats.derivation.bson.AllBsonDecoders._
import mongo4cats.derivation.bson.configured.encoder.auto._
import mongo4cats.derivation.bson.{BsonDecoder, BsonEncoder, BsonValueOps}
import org.bson.BsonBinaryWriter
import org.bson.codecs.EncoderContext
import org.bson.io.BasicOutputBuffer

import java.time.Instant
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit.MILLISECONDS
import java.util.concurrent.TimeUnit.SECONDS

@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@Measurement(iterations = 3)
@Warmup(iterations = 2)
@Timeout(time = 15)
class DerivationWriteBench {

  implicit val bsonConf = mongo4cats.derivation.bson.configured.Configuration.default
    .copy(yoloMode = true)
  // .withDiscriminator("theDiscriminator")
  implicit val circeConf = io.circe.generic.extras.Configuration(
    transformMemberNames = bsonConf.transformMemberNames,
    transformConstructorNames = bsonConf.transformConstructorNames,
    useDefaults = bsonConf.useDefaults,
    discriminator = bsonConf.discriminator
  )

  val bsonEncoder    = BsonEncoder[BenchCC]
  val circeCodec     = deriveCirceCodecProvider[BenchCC].get.get(classOf[BenchCC], null)
  val output         = new BasicOutputBuffer(1000)
  val writer         = new BsonBinaryWriter(output)
  val cc1            = BenchCC()
  val cc2            = BenchCC(BenchST2().some)
  val encoderContext = EncoderContext.builder().build()

  @Benchmark
  def a_writeViaDerivationWithSealedTrait(): Unit = {
    output.truncateToPosition(0)
    bsonEncoder.unsafeBsonEncode(writer, cc2, encoderContext)
  }

  // @Benchmark
  def b_writeViaCirceWithSealedTrait(): Unit = {
    output.truncateToPosition(0)
    circeCodec.encode(writer, cc2, encoderContext)
  }

  // @Benchmark
  def c_writeViaDerivation(): Unit = {
    output.truncateToPosition(0)
    bsonEncoder.unsafeBsonEncode(writer, cc1, encoderContext)
  }

  // @Benchmark
  def d_writeViaCirce(): Unit = {
    output.truncateToPosition(0)
    circeCodec.encode(writer, cc1, encoderContext)
  }
}
