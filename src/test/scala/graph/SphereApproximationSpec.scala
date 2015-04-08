package graph

import graph.Units._
import org.scalatest.{Matchers, FlatSpec}
import Util.triangles

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

class SphereApproximationSpec extends FlatSpec with Matchers {
	import SphereApproximation._
	"Triangles" should "be correctly found in a triangular graph" in {
		val g = Graph[Node, UnDiEdge](
			Label(Vector(Set(1))) ~ Label(Vector(Set(2))),
			Label(Vector(Set(2))) ~ Label(Vector(Set(3))),
			Label(Vector(Set(3))) ~ Label(Vector(Set(1)))
		)
		triangles(g) should equal(Set(
			Set(Label(Vector(Set(1))), Label(Vector(Set(3))), Label(Vector(Set(2))))
		))
	}

	it should "be correctly found in a moderate graph" in {
		val g = Graph[Node, UnDiEdge](
			Label(Vector(Set(1))) ~ Label(Vector(Set(2))),
			Label(Vector(Set(2))) ~ Label(Vector(Set(3))),
			Label(Vector(Set(3))) ~ Label(Vector(Set(1))),
			Label(Vector(Set(2))) ~ Label(Vector(Set(4))),
			Label(Vector(Set(4))) ~ Label(Vector(Set(3))),
			Label(Vector(Set(1))) ~ Label(Vector(Set(5)))
		)
		triangles(g) should equal(
			Set(
				Set(Label(Vector(Set(1))), Label(Vector(Set(3))), Label(Vector(Set(2)))),
				Set(Label(Vector(Set(4))), Label(Vector(Set(3))), Label(Vector(Set(2))))
			)
		)
	}
}
