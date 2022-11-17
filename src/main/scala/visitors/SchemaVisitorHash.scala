package visitors

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
import smithy4s.{Bijection, Hints, Lazy, Refinement, ShapeId}

object SchemaVisitorHash extends SchemaVisitor[Hash] { self =>

  override def primitive[P](
      shapeId: ShapeId,
      hints: Hints,
      tag: Primitive[P]
  ): Hash[P] =
    Hash.fromUniversalHashCode[P]

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
    fields.map(field => forField(field))

    ???

  }

  override def union[U](
      shapeId: ShapeId,
      hints: Hints,
      alternatives: Vector[SchemaAlt[U, _]],
      dispatch: Alt.Dispatcher[Schema, U]
  ): Hash[U] = ???

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
