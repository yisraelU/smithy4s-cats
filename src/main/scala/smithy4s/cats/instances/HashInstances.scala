package smithy4s.cats.instances

import cats.Hash
import cats.implicits.toContravariantOps
import smithy4s.{~>, ByteArray, ShapeId, Timestamp}
import smithy4s.schema.Primitive

import scala.collection.immutable.ArraySeq

trait HashInstances {
  implicit val byteArrayHash: Hash[ByteArray] =
    Hash[ArraySeq[Byte]].contramap { ba =>
      ArraySeq.unsafeWrapArray(ba.array)
    }
  implicit val documentHash: Hash[smithy4s.Document] =
    Hash[String].contramap(_.toString)
  implicit val shapeIdHash: Hash[ShapeId] = Hash[String].contramap(_.toString)
  implicit val timeStampHash: Hash[Timestamp] =
    Hash[Long].contramap(_.epochSecond)
  implicit val primHashPf: ~>[Primitive, Hash] = Primitive.deriving[Hash]

}

object HashInstances extends HashInstances
