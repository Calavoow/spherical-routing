package graph
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Units {
	case class Label(parentalLabel: IndexedSeq[Set[Int]]) {
		lazy val id = parentalLabel.last.head
		lazy val label = {
			parentalLabel.zipAll(parentalLabel.drop(1), Set.empty[Int], Set.empty[Int]).map {
				case (label_k, label_k1) ⇒
					val intersection = label_k intersect label_k1
					if(intersection.nonEmpty) {
						intersection
					} else {
						label_k
					}
			}
		}
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