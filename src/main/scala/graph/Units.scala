package graph

import scala.collection.mutable
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Units {

	object Label {
		def apply(i: Int) = new Label(Vector(Set(LabelEntry(i, 0))))

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
			// Increment distances by one.
			val p1Label = parent1.label.map(_.map { labelEntry ⇒
				labelEntry.copy(distance = labelEntry.distance + 1)
			})
			val p2Label = parent2.label.map(_.map { labelEntry ⇒
				labelEntry.copy(distance = labelEntry.distance + 1)
			})

			val filteredLabels = p1Label.zipAll(p2Label,
				Set(LabelEntry(parent1.id, 1)),
				Set(LabelEntry(parent2.id, 1))
			) map {
				case (l1, l2) ⇒
					val u = l1 union l2
					val minDistance = u.map(_.distance).min
					u.filter(_.distance == minDistance)
			}

			//			val recursiveLabel = if(parent1.layer == parent2.layer) {
			//				parent1.label.zip(parent2.label).map {
			//					case (l1,l2) ⇒ l1 union l2
			//				}
			//			} else {
			//				// The smallest label is of the highest layer
			//				val Seq(parentLower, parentHigher) = Seq(parent1, parent2).sortBy(_.layer)
			//				// Extend the label to the same size of the higher layer label.
			//				val extendedLabel = parentLower.label ++ Vector.fill(parentHigher.label.size - parentLower.label.size)(Set(LabelEntry(parentLower.id,1)))
			//				val newLast = extendedLabel.last + LabelEntry(parentHigher.id, 1)
			//				extendedLabel.updated(extendedLabel.size - 1, newLast)
			//			}

			new Label(filteredLabels :+ Set(LabelEntry(id, 0)))
		}
	}

	case class Label(label: IndexedSeq[Set[LabelEntry]]) {
		lazy val id = label.last.head.id
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
			"[" + label.map(_.mkString("{", ",", "}")).mkString(",") + "]"
		}
	}

	case class LabelEntry(id: Int, distance: Int) {
		override def toString = id.toString
	}

	type Node = Label
	val icosahedron = Graph[Node, UnDiEdge](
		// Outer nodes
		Label(0) ~ Label(1),
		Label(0) ~ Label(2),
		Label(0) ~ Label(3),
		Label(0) ~ Label(4),
		Label(0) ~ Label(5),
		Label(11) ~ Label(6),
		Label(11) ~ Label(7),
		Label(11) ~ Label(8),
		Label(11) ~ Label(9),
		Label(11) ~ Label(10),
		// Inner nodes horizontal
		Label(1) ~ Label(2),
		Label(2) ~ Label(3),
		Label(3) ~ Label(4),
		Label(4) ~ Label(5),
		Label(5) ~ Label(1),
		Label(6) ~ Label(7),
		Label(7) ~ Label(8),
		Label(8) ~ Label(9),
		Label(9) ~ Label(10),
		Label(10) ~ Label(6),
		// Inner nodes vertical forward
		Label(1) ~ Label(6),
		Label(2) ~ Label(7),
		Label(3) ~ Label(8),
		Label(4) ~ Label(9),
		Label(5) ~ Label(10),
		// Inner nodes vertical backward
		Label(1) ~ Label(10),
		Label(2) ~ Label(6),
		Label(3) ~ Label(7),
		Label(4) ~ Label(8),
		Label(5) ~ Label(9)
	)

	val triangle = Graph[Node, UnDiEdge](
		Label(1) ~ Label(2),
		Label(2) ~ Label(3),
		Label(3) ~ Label(1)
	)
}