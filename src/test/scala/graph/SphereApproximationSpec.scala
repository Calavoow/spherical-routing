package graph

import graph.Units._
import org.scalatest.{Matchers, FlatSpec}

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

class SphereApproximationSpec extends FlatSpec with Matchers {
	"Triangles" should "be correctly found in a triangular graph" in {
		val g = Graph[Node, UnDiEdge](
			List(1) ~ List(2),
			List(2) ~ List(3),
			List(3) ~ List(1)
		)
		SphereApproximation.triangles(g) should equal(Set(
			Set(List(1), List(3), List(2))
		))
	}

	it should "be correctly found in a moderate graph" in {
		val g = Graph[Node, UnDiEdge](
			List(1) ~ List(2),
			List(2) ~ List(3),
			List(3) ~ List(1),
			List(2) ~ List(4),
			List(4) ~ List(3),
			List(1) ~ List(5)
		)
		SphereApproximation.triangles(g) should equal(Set(
			Set(List(1), List(3), List(2)),
			Set(List(4), List(3), List(2))
		))
	}
}
