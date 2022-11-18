package tests

import cats.Id
import smithy4s.schema.{
  Alt,
  CollectionTag,
  EnumValue,
  Primitive,
  Schema,
  SchemaAlt,
  SchemaField,
  SchemaVisitor
}
import smithy4s.{Bijection, Hints, Lazy, Refinement, ShapeId}
import tests.companion.IdentityFunc

object companion {
  type IdentityFunc[A] = A => Id[A]
}
object SchemaVisitorId extends SchemaVisitor[IdentityFunc] {
  def liftIdentity[A](a: A): Id[A] = a

  override def primitive[P](
      shapeId: ShapeId,
      hints: Hints,
      tag: Primitive[P]
  ): IdentityFunc[P] =
    liftIdentity[P]

  override def collection[C[`2`], A](
      shapeId: ShapeId,
      hints: Hints,
      tag: CollectionTag[C],
      member: Schema[A]
  ): IdentityFunc[C[A]] =
    liftIdentity[C[A]]

  override def map[K, V](
      shapeId: ShapeId,
      hints: Hints,
      key: Schema[K],
      value: Schema[V]
  ): IdentityFunc[Map[K, V]] =
    liftIdentity[Map[K, V]]

  override def enumeration[E](
      shapeId: ShapeId,
      hints: Hints,
      values: List[EnumValue[E]],
      total: E => EnumValue[E]
  ): IdentityFunc[E] = liftIdentity[E]

  override def struct[S](
      shapeId: ShapeId,
      hints: Hints,
      fields: Vector[SchemaField[S, _]],
      make: IndexedSeq[Any] => S
  ): IdentityFunc[S] = liftIdentity[S]

  override def union[U](
      shapeId: ShapeId,
      hints: Hints,
      alternatives: Vector[SchemaAlt[U, _]],
      dispatch: Alt.Dispatcher[Schema, U]
  ): IdentityFunc[U] = liftIdentity[U]

  override def biject[A, B](
      schema: Schema[A],
      bijection: Bijection[A, B]
  ): IdentityFunc[B] = liftIdentity[B]

  override def refine[A, B](
      schema: Schema[A],
      refinement: Refinement[A, B]
  ): IdentityFunc[B] = liftIdentity[B]

  override def lazily[A](suspend: Lazy[Schema[A]]): IdentityFunc[A] =
    liftIdentity[A]
}
