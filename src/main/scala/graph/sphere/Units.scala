package graph.sphere

import graph.Util.{ID, Layered}

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

object Units {
	type Sphere = Graph[Node, UnDiEdge]

	object Label {
		def apply(i: Int) = new Label(Vector(Set(i)), 0, None)

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
		def apply(parent1: Label, parent2: Label, id: Int, nodes : Set[Label]) = {
			// Zip both labels, until one of the labels reaches the base graph.
			val unfilteredLabel = parent1.label.zip(parent2.label).map {
				case (l1, l2) =>
					l1 union l2
			}
//			println(s"$id. P1: $parent1 / P2: $parent2")
			// Calculate all previous elements that have occurred at each index.
//			val prefixUnion = unfilteredLabel.scanLeft(Set[Int]())(_ union _)
			val filteredLabel = unfilteredLabel.foldLeft((List[Set[Label]](), Set[Int]())){
				case ((filteredEls, prefixUnion), unfilteredEl) =>
					val removedDuplicates = unfilteredEl -- prefixUnion
					val vertices: Set[Label] = nodes.filter(n => removedDuplicates.contains(n.id))
					val nonSimpleVertices = vertices.filterNot { vertex =>
						// Neither parent must already be in the label, nor in the current label.
						// If parents = None, then there are no parents already in the label.
						vertex.parents.exists {
							case (p1,p2) =>
								prefixUnion(p1.id) || prefixUnion(p2.id) || removedDuplicates(p1.id) || removedDuplicates(p2.id)
						}
					}
					(nonSimpleVertices :: filteredEls, prefixUnion union unfilteredEl)
			}._1.reverse
//			println(filteredLabel)
//			val filteredLabel = unfilteredLabel.zip(prefixUnion).map {
//				case (unfilteredEl, prefixSet) ⇒
//					val removedDuplicates = unfilteredEl -- prefixSet
//					val vertices: Set[Label] = nodes.filter(n => removedDuplicates.contains(n.id))
//					vertices.filterNot { vertex =>
//						// Neither parent must already be in the label, nor in the current label.
//						val (p1, p2) = vertex.parents
//						prefixSet(p1.id) || prefixSet(p2.id) || removedDuplicates(p1.id) || removedDuplicates(p2.id)
//					}
//			}

			// Remove non-simple vertices that do not have a parent in the label
			val filterNoParents = filteredLabel.scanRight(nodes){
				case (labeli, labeli1) =>
					labeli.filter { vertex =>
						// Get all vertices from label_i that have a parent in label_{i+1}
						vertex.parents.map {
							case (p1,p2) => labeli1(p1) || labeli1(p2)
						}.getOrElse(true) // Always keep outer layer vertices.
					}
			}.dropRight(1).toIndexedSeq
//			println(filterNoParents)
			new Label(Set(id) +: filterNoParents.map(_.map(_.id)), parent1.layer.max(parent2.layer) + 1, Some(parent1, parent2))
		}


		implicit object LayeredLabel extends Layered[Label] {
			def layer(x: Label, nrLayers: Int) = {
				// nrLayers - 1 - x.layer
				x.layer
			}
		}
		implicit object IdLabel extends ID[Label] {
			override def id(x: Label) : Int = x.id
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
	case class Label(label: IndexedSeq[Set[Int]], layer: Int, parents: Option[(Label, Label)]) {
		/**
		 * The id of this node.
		 *
		 * Equivalent to the last element of the label.
		 */
		lazy val id = label.head.head

		override def toString = {
			"[" + label.map(_.mkString("{", ",", "}")).mkString(",") + "]_" + layer
		}

		override def hashCode : Int = {
			id.hashCode()
		}

		override def equals(other: Any): Boolean = other match {
			case that : Label => that.id == id
			case _ => false
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
		Label(0) ~ Label(1),
		Label(1) ~ Label(2),
		Label(2) ~ Label(0)
	)
}