package smithy4s.cats.instances

import cats.Hash
import cats.implicits.toContravariantOps
import smithy4s.{~>, ByteArray, ShapeId, Timestamp}
import smithy4s.schema.Primitive

trait HashInstances {
  implicit val byteArrayHash: Hash[ByteArray] =
    Hash[Seq[Byte]].contramap(_.array.toSeq)
  implicit val documentHash: Hash[smithy4s.Document] =
    Hash[String].contramap(_.toString)
  implicit val shapeIdHash: Hash[ShapeId] = Hash[String].contramap(_.toString)
  implicit val timeStampHash: Hash[Timestamp] =
    Hash[Long].contramap(_.epochSecond)
  implicit val primHashPf: ~>[Primitive, Hash] = Primitive.deriving[Hash]

}

object HashInstances extends HashInstances
