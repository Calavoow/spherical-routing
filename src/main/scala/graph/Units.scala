package graph

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

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

			// Filter out labels which are too far.
			val filteredLabels = p1Label.zipAll(p2Label,
				Set(LabelEntry(parent1.id, 1)),
				Set(LabelEntry(parent2.id, 1))
			) map {
				case (l1, l2) ⇒
					val u = l1 union l2
					// Take the minimum distance for this label entry (index i)
					val minDistance = u.map(_.distance).min
					// Remove all label entries (at i) which have a larger distance.
					u.filter(_.distance == minDistance)
			}

			// Append own id at the end of the new label.
			new Label(filteredLabels :+ Set(LabelEntry(id, 0)))
		}
	}

	/**
	 * The label of a node.
	 *
	 * Every node has a label, which consists of its parents, and the parents parent, etc.
	 * This recursive label is filtered in such a way that at index k, you can find a set of ids of nodes
	 * that can be routed towards if you are on layer k.
	 * This is also used to easily find the closest common ancestor.
	 * @param label The label of this node.
	 */
	case class Label(label: IndexedSeq[Set[LabelEntry]]) {
		/**
		 * The id of this node.
		 *
		 * Equivalent to the last element of the label.
		 */
		lazy val id = label.last.head.id

		/**
		 * The layer of this node.
		 *
		 * Equivalent to the length of the label.
		 * @return The layer, starting at 0.
		 */
		def layer = label.size - 1

		override def toString = {
			"[" + label.map(_.mkString("{", ",", "}")).mkString(",") + "]"
		}
	}

	/**
	 * An entry in the set of label ids.
	 *
	 * @param id The id of the node.
	 * @param distance The distance to this node, from the node that has the id in its label.
	 */
	case class LabelEntry(id: Int, distance: Int) {
		override def toString = id.toString
	}

	type Node = Label
	/**
	 * The icosahedron manually encoded as a Graph object.
	 */
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

	/**
	 * The triangle encoded as a graph object.
	 */
	val triangle = Graph[Node, UnDiEdge](
		Label(1) ~ Label(2),
		Label(2) ~ Label(3),
		Label(3) ~ Label(1)
	)
}