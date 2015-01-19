package graph
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Units {
	case class Label(level: Int, label: Int)

	type Node = Label
	val icosahedron = Graph[Node, UnDiEdge](
		// Outer nodes
		Label(0,0) ~ Label(0,1),
		Label(0,0) ~ Label(0,2),
		Label(0,0) ~ Label(0,3),
		Label(0,0) ~ Label(0,4),
		Label(0,0) ~ Label(0,5),
		Label(0,11) ~ Label(0,6),
		Label(0,11) ~ Label(0,7),
		Label(0,11) ~ Label(0,8),
		Label(0,11) ~ Label(0,9),
		Label(0,11) ~ Label(0,10),
		// Inner nodes horizontal
		Label(0,1) ~ Label(0,2),
		Label(0,2) ~ Label(0,3),
		Label(0,3) ~ Label(0,4),
		Label(0,4) ~ Label(0,5),
		Label(0,5) ~ Label(0,1),
		Label(0,6) ~ Label(0,7),
		Label(0,7) ~ Label(0,8),
		Label(0,8) ~ Label(0,9),
		Label(0,9) ~ Label(0,10),
		Label(0,10) ~ Label(0,6),
		// Inner nodes vertical forward
		Label(0,1) ~ Label(0,6),
		Label(0,2) ~ Label(0,7),
		Label(0,3) ~ Label(0,8),
		Label(0,4) ~ Label(0,9),
		Label(0,5) ~ Label(0,10),
		// Inner nodes vertical backward
		Label(0,1) ~ Label(0,10),
		Label(0,2) ~ Label(0,6),
		Label(0,3) ~ Label(0,7),
		Label(0,4) ~ Label(0,8),
		Label(0,5) ~ Label(0,9)
	)
}