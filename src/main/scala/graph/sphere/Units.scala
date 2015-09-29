package graph.sphere

import graph.Util.{ID, Layered}

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

object Units {
	type Sphere = Graph[Node, UnDiEdge]

	object SphereNode {
		def apply(i: Int) = new SphereNode(i, 0)

		def apply(id: Int, parent1: SphereNode, parent2: SphereNode) = {
			val layer = Math.max(parent1.layer, parent2.layer) + 1
			new SphereNode(id, layer)
		}

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
		/*
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
		*/

		def parents(g: Sphere)(v: g.NodeT) = {
			v.innerNodeTraverser.withMaxDepth(1).filter(_.layer < v.layer)
		}

		implicit object LayeredLabel extends Layered[SphereNode] {
			def layer(x: SphereNode, nrLayers: Int) = {
				// nrLayers - 1 - x.layer
				x.layer
			}
		}
		implicit object IdLabel extends ID[SphereNode] {
			override def id(x: SphereNode) : Int = x.id
		}
	}

	/**
	 * The label of a node.
	 *
	 * Every node has a label, which consists of its parents, and the parents parent, etc.
	 * This recursive label is filtered in such a way that at index k, you can find a set of ids of nodes
	 * that can be routed towards if you are on layer k.
	 * This is also used to easily find the closest common ancestor.
	 */
	case class SphereNode(id: Int, layer: Int) {
		override def toString = {
			s"${id}_$layer"
		}

		override def equals(other: Any): Boolean = other match {
			case that : SphereNode => that.id == id
			case _ => false
		}

		override def hashCode(): Int = id.hashCode()
	}

	type Node = SphereNode
	/**
	 * The icosahedron manually encoded as a Graph object.
	 */
	val icosahedron = Graph[Node, UnDiEdge](
		// Outer nodes
		SphereNode(0) ~ SphereNode(1),
		SphereNode(0) ~ SphereNode(2),
		SphereNode(0) ~ SphereNode(3),
		SphereNode(0) ~ SphereNode(4),
		SphereNode(0) ~ SphereNode(5),
		SphereNode(11) ~ SphereNode(6),
		SphereNode(11) ~ SphereNode(7),
		SphereNode(11) ~ SphereNode(8),
		SphereNode(11) ~ SphereNode(9),
		SphereNode(11) ~ SphereNode(10),
		// Inner nodes horizontal
		SphereNode(1) ~ SphereNode(2),
		SphereNode(2) ~ SphereNode(3),
		SphereNode(3) ~ SphereNode(4),
		SphereNode(4) ~ SphereNode(5),
		SphereNode(5) ~ SphereNode(1),
		SphereNode(6) ~ SphereNode(7),
		SphereNode(7) ~ SphereNode(8),
		SphereNode(8) ~ SphereNode(9),
		SphereNode(9) ~ SphereNode(10),
		SphereNode(10) ~ SphereNode(6),
		// Inner nodes vertical forward
		SphereNode(1) ~ SphereNode(6),
		SphereNode(2) ~ SphereNode(7),
		SphereNode(3) ~ SphereNode(8),
		SphereNode(4) ~ SphereNode(9),
		SphereNode(5) ~ SphereNode(10),
		// Inner nodes vertical backward
		SphereNode(1) ~ SphereNode(10),
		SphereNode(2) ~ SphereNode(6),
		SphereNode(3) ~ SphereNode(7),
		SphereNode(4) ~ SphereNode(8),
		SphereNode(5) ~ SphereNode(9)
	)

	/**
	 * The triangle encoded as a graph object.
	 */
	val triangle = Graph[Node, UnDiEdge](
		SphereNode(0) ~ SphereNode(1),
		SphereNode(1) ~ SphereNode(2),
		SphereNode(2) ~ SphereNode(0)
	)
}