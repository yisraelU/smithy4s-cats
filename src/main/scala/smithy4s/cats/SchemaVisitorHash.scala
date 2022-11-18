package smithy4s.cats

import cats.Hash
import cats.implicits.{
  catsKernelStdHashForList,
  catsKernelStdHashForOption,
  toContravariantOps
}
import smithy4s.schema.{
  Alt,
  CollectionTag,
  EnumValue,
  Field,
  Primitive,
  Schema,
  SchemaAlt,
  SchemaField,
  SchemaVisitor
}
import smithy4s.{
  ~>,
  Bijection,
  Timestamp,
  ByteArray,
  Existential,
  Hints,
  Lazy,
  Refinement,
  ShapeId
}
import smithy4s.cats.SchemaVisitorHash._
class SchemaVisitorHash extends SchemaVisitorEq with SchemaVisitor[Hash] {
  self =>

  override def primitive[P](
      shapeId: ShapeId,
      hints: Hints,
      tag: Primitive[P]
  ): Hash[P] = primHashPf(tag)

  override def collection[C[`2`], A](
      shapeId: ShapeId,
      hints: Hints,
      tag: CollectionTag[C],
      member: Schema[A]
  ): Hash[C[A]] = {
    implicit val memberHash: Hash[A] = self(member)
    tag match {
      case CollectionTag.ListTag       => Hash[List[A]]
      case CollectionTag.SetTag        => Hash[Set[A]]
      case CollectionTag.VectorTag     => Hash[Vector[A]]
      case CollectionTag.IndexedSeqTag => Hash[Seq[A]].contramap(_.toIndexedSeq)
    }
  }

  override def map[K, V](
      shapeId: ShapeId,
      hints: Hints,
      key: Schema[K],
      value: Schema[V]
  ): Hash[Map[K, V]] = {
    implicit val keyHash: Hash[K] = self(key)
    implicit val valueHash: Hash[V] = self(value)
    Hash[Map[K, V]]
  }

  override def enumeration[E](
      shapeId: ShapeId,
      hints: Hints,
      values: List[EnumValue[E]],
      total: E => EnumValue[E]
  ): Hash[E] = {
    implicit val enumValueHash: Hash[EnumValue[E]] =
      Hash[String].contramap(_.stringValue)
    Hash[EnumValue[E]].contramap(total)
  }

  override def struct[S](
      shapeId: ShapeId,
      hints: Hints,
      fields: Vector[SchemaField[S, _]],
      make: IndexedSeq[Any] => S
  ): Hash[S] = {
    new Hash[S] {
      override def hash(x: S): Int = {
        def forField[A2](field: Field[Schema, S, A2]): Hash[S] = {
          val hashField: Hash[A2] =
            field.foldK(new Field.FolderK[Schema, S, Hash]() {
              override def onRequired[A](
                  label: String,
                  instance: Schema[A],
                  get: S => A
              ): Hash[A] = self(instance)

              override def onOptional[A](
                  label: String,
                  instance: Schema[A],
                  get: S => Option[A]
              ): Hash[Option[A]] = {
                implicit val hashA: Hash[A] = self(instance)
                Hash[Option[A]]
              }
            })
          hashField.contramap(field.get)
        }
        fields.map(field => forField(field)).hashCode()
      }
      override def eqv(x: S, y: S): Boolean =
        self.struct(shapeId, hints, fields, make).eqv(x, y)

    }
  }

  override def union[U](
      shapeId: ShapeId,
      hints: Hints,
      alternatives: Vector[SchemaAlt[U, _]],
      dispatch: Alt.Dispatcher[Schema, U]
  ): Hash[U] = {
    new Hash[U] {

      override def hash(x: U): Int = {
        type F[A] = SchemaAlt[U, A]
        val compileAlt = {
          new (F ~> Hash) {
            override def apply[A](fa: SchemaAlt[U, A]): Hash[A] =
              fa.instance.compile(self)
          }
        }
        val precomputed =
          compileAlt.unsafeCache(alternatives.map(Existential.wrap(_)))

        def hash[A](altV: Alt.SchemaAndValue[U, A]): Int = {
          precomputed(altV.alt).hash(
            altV.value
          )

        }
        hash(dispatch.underlying(x))
      }
      override def eqv(x: U, y: U): Boolean =
        self.union(shapeId, hints, alternatives, dispatch).eqv(x, y)
    }
  }

  override def biject[A, B](
      schema: Schema[A],
      bijection: Bijection[A, B]
  ): Hash[B] = {
    implicit val hashA: Hash[A] = self(schema)
    Hash[A].contramap(bijection.from)
  }

  override def refine[A, B](
      schema: Schema[A],
      refinement: Refinement[A, B]
  ): Hash[B] = {
    implicit val hashA: Hash[A] = self(schema)
    Hash[A].contramap(refinement.from)
  }

  override def lazily[A](suspend: Lazy[Schema[A]]): Hash[A] = {
    implicit val hashA: Lazy[Hash[A]] = suspend.map(self(_))
    new Hash[A] {
      override def hash(x: A): Int = hashA.value.hash(x)
      override def eqv(x: A, y: A): Boolean = hashA.value.eqv(x, y)
    }
  }
}

object SchemaVisitorHash {
  implicit val byteArrayHash: Hash[ByteArray] =
    Hash[Seq[Byte]].contramap(_.array.toSeq)
  implicit val documentHash: Hash[smithy4s.Document] =
    Hash[String].contramap(_.toString)
  implicit val shapeIdHash: Hash[ShapeId] = Hash[String].contramap(_.toString)
  implicit val timeStampHash: Hash[Timestamp] =
    Hash[Long].contramap(_.epochSecond)
  implicit val primHashPf: ~>[Primitive, Hash] = Primitive.deriving[Hash]
}
