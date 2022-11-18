package smithy4s.cats.instances

import cats.Show
import cats.implicits.toContravariantOps
import smithy4s.schema.Primitive
import smithy4s.{ByteArray, Document, ShapeId, Timestamp}

trait ShowInstances {

  implicit val sId: Show[ShapeId] = Show.fromToString
  implicit val byteArray: Show[ByteArray] =
    Show[Seq[Byte]].contramap(_.array.toSeq)
  implicit val document: Show[Document] = Show.fromToString
  implicit val ts: Show[Timestamp] = Show.fromToString
  implicit val primShowPf = Primitive.deriving[Show]

}

object ShowInstances extends ShowInstances
