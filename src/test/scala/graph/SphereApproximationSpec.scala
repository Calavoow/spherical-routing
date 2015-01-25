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
			Label(0,1) ~ Label(0,2),
			Label(0,2) ~ Label(0,3),
			Label(0,3) ~ Label(0,1)
		)
		triangles(g) should equal(Set(
			Set(Label(0,1), Label(0,3), Label(0,2))
		))
	}

	it should "be correctly found in a moderate graph" in {
		val g = Graph[Node, UnDiEdge](
			Label(0,1) ~ Label(0,2),
			Label(0,2) ~ Label(0,3),
			Label(0,3) ~ Label(0,1),
			Label(0,2) ~ Label(0,4),
			Label(0,4) ~ Label(0,3),
			Label(0,1) ~ Label(0,5)
		)
		triangles(g) should equal(
			Set(
				Set(Label(0,1), Label(0,3), Label(0,2)),
				Set(Label(0,4), Label(0,3), Label(0,2))
			)
		)
	}
}
