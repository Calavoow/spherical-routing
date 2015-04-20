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

	"Subdivison" should "subdivide a triangle" in {
		val g = Units.triangle
		val g1 = SphereApproximation.subdivide(g)
		g1.nodes.size should equal(6)
		g1.edges.size should equal(12)
		g1.nodes should contain( Label(1) )
		g1.nodes should contain( Label(2) )
		g1.nodes should contain( Label(3) )
		g1.nodes.find(_.parentalLabel.head == Set(1,2)) should be('defined)
		g1.nodes.find(_.parentalLabel.head == Set(2,3)) should be('defined)
		g1.nodes.find(_.parentalLabel.head == Set(1,3)) should be('defined)
	}

	it should "always divide in the same way" in {
		val graphs = for(_ <- (1 to 100)) yield {
			SphereApproximation.repeatedSubdivision(icosahedron).drop(3).next()
		}

		graphs.sliding(2).foreach {
			case Seq(g1, g2) => g1 should equal(g2)
		}
	}
}
