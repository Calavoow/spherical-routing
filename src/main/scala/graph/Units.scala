package graph
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Units {
	type Node = List[Int]
	val icosahedron = Graph[Node, UnDiEdge](
		// Outer nodes
		List(0) ~ List(1),
		List(0) ~ List(2),
		List(0) ~ List(3),
		List(0) ~ List(4),
		List(0) ~ List(5),
		List(11) ~ List(6),
		List(11) ~ List(7),
		List(11) ~ List(8),
		List(11) ~ List(9),
		List(11) ~ List(10),
		// Inner nodes horizontal
		List(1) ~ List(2),
		List(2) ~ List(3),
		List(3) ~ List(4),
		List(4) ~ List(5),
		List(5) ~ List(1),
		List(6) ~ List(7),
		List(7) ~ List(8),
		List(8) ~ List(9),
		List(9) ~ List(10),
		List(10) ~ List(6),
		// Inner nodes vertical forward
		List(1) ~ List(6),
		List(2) ~ List(7),
		List(3) ~ List(8),
		List(4) ~ List(9),
		List(5) ~ List(10),
		// Inner nodes vertical backward
		List(1) ~ List(10),
		List(2) ~ List(6),
		List(3) ~ List(7),
		List(4) ~ List(8),
		List(5) ~ List(9)
	)
}