package graph.sphere

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

object Units {

	object Label {
		def apply(i: Int) = new Label(Vector(Set(i)), 0)

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
			// Zip both labels, until one of the labels reaches the base graph.
			val unfilteredLabel = parent1.label.zip(parent2.label).toIndexedSeq.map {
				case (l1, l2) =>
					l1 union l2
					// Remove parent ids that have already occurred in the label (at position 1)
					// This will prevent a tree to be added twice.
//					if(index > 0) union -- Set(parent1.id, parent2.id)
//					else union
			}
			// Calculate all previous elements that have occurred at each index.
			val prefixUnion = unfilteredLabel.scanLeft(Set[Int]())(_ union _)
			val filteredLabel = unfilteredLabel.zip(prefixUnion).map {
				case (unfilteredEl, prefixSet) ⇒ unfilteredEl -- prefixSet
			}
			new Label(Set(id) +: filteredLabel, parent1.layer.max(parent2.layer) + 1)
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
	case class Label(label: IndexedSeq[Set[Int]], layer: Int) {
		/**
		 * The id of this node.
		 *
		 * Equivalent to the last element of the label.
		 */
		lazy val id = label.head.head

		override def toString = {
			"[" + label.map(_.mkString("{", ",", "}")).mkString(",") + "]_" + layer
		}
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