package mongo4cats.bench

import cats.syntax.all._
import mongo4cats.bench.BenchMyCC.{fastestMyCCBsonEncoder, slowMyCCBsonEncoder}
import mongo4cats.bench.BsonEncoderInstanceBench._
import mongo4cats.bench.DerivationWriteBench.{encoderContext, output}
import mongo4cats.derivation.bson.BsonEncoder
import mongo4cats.derivation.bson.configured.encoder.auto._
import org.bson.{BsonBinaryWriter, BsonDocument, BsonDocumentWriter}
import org.bson.codecs.EncoderContext
import org.bson.io.BasicOutputBuffer
import mongo4cats.derivation.bson.AllBsonEncoders._
import mongo4cats.derivation.bson.AllBsonDecoders._
import mongo4cats.derivation.bson.configured.Configuration
import mongo4cats.derivation.bson.configured.encoder.auto._

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@Warmup(iterations = 2)
@Measurement(iterations = 3)
@Timeout(time = 15)
class BsonEncoderInstanceBench {

  @Setup
  def setup() = {
    def encodeToDoc(bsonEncoder: BsonEncoder[BenchTenMyCC]) = {
      val doc = new BsonDocument()
      bsonEncoder.unsafeBsonEncode(new BsonDocumentWriter(doc), tenMyCC, encoderContext)
      doc
    }

    val fastDoc = encodeToDoc(fastBsonEncoder)
    val slowDoc = encodeToDoc(slowBsonEncoder)
    val msg =
      s"""|fast    : $fastDoc
          |slow    : $slowDoc
          |""".stripMargin
    // println(fastDoc.toJson)

    if (
      fastDoc.toJson != """{"myCC0": {"my-cc": {"my-value": "abc"}}, "myCC1": {"my-cc": {"my-value": "abc"}}, "myCC2": {"my-cc": {"my-value": "abc"}}, "myCC3": {"my-cc": {"my-value": "abc"}}, "myCC4": {"my-cc": {"my-value": "abc"}}, "myCC5": {"my-cc": {"my-value": "abc"}}, "myCC6": {"my-cc": {"my-value": "abc"}}, "myCC7": {"my-cc": {"my-value": "abc"}}, "myCC8": {"my-cc": {"my-value": "abc"}}, "myCC9": {"my-cc": {"my-value": "abc"}}}"""
    ) throw new Throwable(s"Bad encoder\n$msg")
    if (fastDoc != slowDoc) throw new Throwable(s"Bad encoder\n$msg")
  }

  @Benchmark
  def a_fastEncoder(): Unit = {
    output.truncateToPosition(0)
    fastBsonEncoder.unsafeBsonEncode(writer, tenMyCC, encoderContext)
  }

  @Benchmark
  def b_slowEncoder(): Unit = {
    output.truncateToPosition(0)
    slowBsonEncoder.unsafeBsonEncode(writer, tenMyCC, encoderContext)
  }
}

object BsonEncoderInstanceBench {
  val fastBsonEncoder: BsonEncoder[BenchTenMyCC] = {
    implicit val bsonConf = Configuration.default
    implicit val encoder  = fastestMyCCBsonEncoder
    BsonEncoder[BenchTenMyCC]
  }

  val slowBsonEncoder: BsonEncoder[BenchTenMyCC] = {
    implicit val bsonConf = Configuration.default
    implicit val encoder  = slowMyCCBsonEncoder
    BsonEncoder[BenchTenMyCC]
  }

  val writer  = new BsonBinaryWriter(output)
  val tenMyCC = BenchTenMyCC()
}
