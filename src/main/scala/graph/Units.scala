package graph
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Units {
	object Label {
		def apply(i : Int) = new Label(Vector(Set(i)))

		/**
		 * Construct a label given two parents and an ID.
		 *
		 * The order of the parents is irrelevant.
		 *
		 * @param parent1 The first parent
		 * @param parent2 The second parent
		 * @param id The ID.
		 * @return A new Label instance, with the label properly filtered.
		 */
		def apply(parent1: Label, parent2: Label, id: Int) = {
			val recursiveLabel = if(parent1.layer == parent2.layer) {
				parent1.label.zip(parent2.label).map {
					case (l1,l2) ⇒ l1 union l2
				}
			} else {
				// The smallest label is of the highest layer
				val Seq(parentLower, parentHigher) = Seq(parent1, parent2).sortBy(_.layer)
				// Extend the label to the same size of the higher layer label.
				val extendedLabel = parentLower.label ++ Vector.fill(parentHigher.label.size - parentLower.label.size)(Set(parentLower.id))
				val newLast = extendedLabel.last + parentHigher.id
				extendedLabel.updated(extendedLabel.size - 1, newLast)
			}

			new Label(recursiveLabel :+ Set(id))
		}
	}

	case class Label(label: IndexedSeq[Set[Int]]) {
		lazy val id = label.last.head
//		lazy val label = {
//			parentalLabel.zipAll(parentalLabel.drop(1), Set.empty[Int], Set.empty[Int]).map {
//				case (label_k, label_k1) ⇒
//					val intersection = label_k intersect label_k1
//					if(intersection.nonEmpty) {
//						intersection
//					} else {
//						label_k
//					}
//			}
//		}

		def layer = label.size - 1
		override def toString = {
			"[" + label.map(_.mkString("{",",","}")).mkString(",") + "]"
		}
	}

	type Node = Label
	val icosahedron = Graph[Node, UnDiEdge](
		// Outer nodes
		Label(Vector(Set(0))) ~ Label(Vector(Set(1))),
		Label(Vector(Set(0))) ~ Label(Vector(Set(2))),
		Label(Vector(Set(0))) ~ Label(Vector(Set(3))),
	    Label(Vector(Set(0))) ~ Label(Vector(Set(4))),
	    Label(Vector(Set(0))) ~ Label(Vector(Set(5))),
		Label(Vector(Set(11))) ~ Label(Vector(Set(6))),
		Label(Vector(Set(11))) ~ Label(Vector(Set(7))),
		Label(Vector(Set(11))) ~ Label(Vector(Set(8))),
		Label(Vector(Set(11))) ~ Label(Vector(Set(9))),
		Label(Vector(Set(11))) ~ Label(Vector(Set(10))),
		// Inner nodes horizontal
		Label(Vector(Set(1))) ~ Label(Vector(Set(2))),
		Label(Vector(Set(2))) ~ Label(Vector(Set(3))),
		Label(Vector(Set(3))) ~ Label(Vector(Set(4))),
		Label(Vector(Set(4))) ~ Label(Vector(Set(5))),
		Label(Vector(Set(5))) ~ Label(Vector(Set(1))),
		Label(Vector(Set(6))) ~ Label(Vector(Set(7))),
		Label(Vector(Set(7))) ~ Label(Vector(Set(8))),
		Label(Vector(Set(8))) ~ Label(Vector(Set(9))),
		Label(Vector(Set(9))) ~ Label(Vector(Set(10))),
		Label(Vector(Set(10))) ~ Label(Vector(Set(6))),
		// Inner nodes vertical forward
		Label(Vector(Set(1))) ~ Label(Vector(Set(6))),
		Label(Vector(Set(2))) ~ Label(Vector(Set(7))),
		Label(Vector(Set(3))) ~ Label(Vector(Set(8))),
		Label(Vector(Set(4))) ~ Label(Vector(Set(9))),
		Label(Vector(Set(5))) ~ Label(Vector(Set(10))),
		// Inner nodes vertical backward
		Label(Vector(Set(1))) ~ Label(Vector(Set(10))),
		Label(Vector(Set(2))) ~ Label(Vector(Set(6))),
		Label(Vector(Set(3))) ~ Label(Vector(Set(7))),
		Label(Vector(Set(4))) ~ Label(Vector(Set(8))),
		Label(Vector(Set(5))) ~ Label(Vector(Set(9)))
	)

	val triangle = Graph[Node, UnDiEdge](
		Label(Vector(Set(1))) ~ Label(Vector(Set(2))),
		Label(Vector(Set(2))) ~ Label(Vector(Set(3))),
		Label(Vector(Set(3))) ~ Label(Vector(Set(1)))
	)
}