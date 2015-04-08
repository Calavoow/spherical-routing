package graph
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Units {
	case class Label(label: IndexedSeq[Set[Int]]) {
		def id = label.last.head
	}

	type Node = Label
	val icosahedron = Graph[Label, UnDiEdge](
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
}