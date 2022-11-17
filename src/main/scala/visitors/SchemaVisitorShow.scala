package visitors

import cats.Show
import cats.implicits.toContravariantOps
import smithy4s._
import smithy4s.schema.{Schema, _}

object SchemaVisitorShow {
  implicit val sId: Show[ShapeId] = new Show[ShapeId]() {
    def show(s: ShapeId): String = s"${s.namespace}#${s.name}"
  }
  implicit val byteArray: Show[ByteArray] = new Show[ByteArray]() {
    def show(s: ByteArray): String = s"ByteArray(length=${s.array.length})"
  }
  implicit val document: Show[Document] = new Show[Document]() {
    def show(s: Document): String = s.show
  }
  implicit val ts: Show[Timestamp] = new Show[Timestamp]() {
    def show(s: Timestamp): String = s.epochSecond.toString()
  }

  val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
    self =>
    private val primShowPf = Primitive.deriving[Show]

    override def primitive[P](
        shapeId: ShapeId,
        hints: Hints,
        tag: Primitive[P]
    ): Show[P] = Show.show { p =>
      val valueShow = primShowPf(tag).show(p)
      s"Primitive { value = $valueShow }"
    }

    override def collection[C[`2`], A](
        shapeId: ShapeId,
        hints: Hints,
        tag: CollectionTag[C],
        member: Schema[A]
    ): Show[C[A]] = {
      val showSchemaA = sv(member)
      tag match {
        case CollectionTag.ListTag =>
          Show.show { l =>
            val showList = l.map(showSchemaA.show _).mkString(", ")
            s"List { values = [ $showList ] }"
          }
        case CollectionTag.SetTag =>
          Show.show { s =>
            val showSet = s.map(showSchemaA.show _).mkString(", ")
            s"Set { values = [ $showSet ] }"
          }
        case CollectionTag.VectorTag =>
          Show.show { v =>
            val showVector = v.map(showSchemaA.show _).mkString(", ")
            s"Vector { values = [ $showVector ] }"
          }
        case CollectionTag.IndexedSeqTag =>
          Show.show { is =>
            val showIndexedSeq = is.map(showSchemaA.show _).mkString(", ")
            s"IndexedSeq { values = [ $showIndexedSeq ] }"
          }
      }
    }

    override def union[U](
        shapeId: ShapeId,
        hints: Hints,
        alternatives: Vector[SchemaAlt[U, _]],
        dispatch: Alt.Dispatcher[Schema, U]
    ): Show[U] = {
      type F[A] = SchemaAlt[U, A]
      val compileAlt = {
        new (F ~> Show) {
          override def apply[A](fa: SchemaAlt[U, A]): Show[A] =
            fa.instance.compile(sv)
        }
      }

      val precomputed =
        compileAlt.unsafeCache(alternatives.map(Existential.wrap(_)))

      def showIt[A](altV: Alt.SchemaAndValue[U, A]): String =
        precomputed(altV.alt).show(altV.value)

      Show.show { u =>
        val withV = dispatch.underlying(u)
        val value = showIt(withV)
        s"Union { shapeId = ${sId.show(shapeId)}, value = $value }"
      }
    }

    override def biject[A, B](
        schema: Schema[A],
        bijection: Bijection[A, B]
    ): Show[B] = {
      val showA = sv(schema)
      Show.show { b =>
        val a = bijection.from(b)
        val showAValue = showA.show(a)
        s"Bijection { value = $showAValue }"
      }
    }

    override def refine[A, B](
        schema: Schema[A],
        refinement: Refinement[A, B]
    ): Show[B] =
      sv(schema).contramap(refinement.from)

    override def map[K, V](
        shapeId: ShapeId,
        hints: Hints,
        key: Schema[K],
        value: Schema[V]
    ): Show[Map[K, V]] = Show.show { m =>
      val showKey = sv(key)
      val showValue = sv(value)
      val values = m
        .map { case (k, v) => s"${showKey.show(k)} -> ${showValue.show(v)}" }
        .mkString("[", ", ", "]")
      s"Map { values = $values }"
    }

    override def enumeration[E](
        shapeId: ShapeId,
        hints: Hints,
        values: List[EnumValue[E]],
        total: E => EnumValue[E]
    ): Show[E] = Show.show { e =>
      val value = total(e)
      s"Enum { value = ${value.stringValue} }"
    }

    override def struct[S](
        shapeId: ShapeId,
        hints: Hints,
        fields: Vector[SchemaField[S, _]],
        make: IndexedSeq[Any] => S
    ): Show[S] = {
      def compileField[A](schemaField: SchemaField[S, A]): S => String = {
        val folder = new Field.FolderK[Schema, S, Show]() {
          override def onRequired[AA](
              label: String,
              instance: Schema[AA],
              get: S => AA
          ): Show[AA] = sv(instance)

          override def onOptional[AA](
              label: String,
              instance: Schema[AA],
              get: S => Option[AA]
          ): Show[Option[AA]] = Show.show {
            case None =>
              s"None"
            case Some(value) =>
              val show = sv(instance)
              s"Optional { ${show.show(value)} }"
          }
        }
        val showField = schemaField.foldK(folder)
        s => showField.show(schemaField.get(s))
      }

      val functions = fields.map { f => compileField(f) }
      Show.show { s =>
        val values = functions
          .map(f => f(s))
          .mkString("[", ", ", "]")
        s"Struct { shapeId = ${sId.show(shapeId)}, fields = $values }"
      }
    }

    override def lazily[A](suspend: Lazy[Schema[A]]): Show[A] = Show.show[A] {
      val ss = suspend.map {
        sv(_)
      }
      a => s"Lazy = ${ss.value.show(a)}"
    }

  }
}
