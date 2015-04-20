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

	it should "subdivide a triangle of different layers" in {
		val g = Graph[Node, UnDiEdge](
			Label(Vector(Set(1))) ~ Label(Vector(Set(1,2), Set(4))),
			Label(Vector(Set(1))) ~ Label(Vector(Set(1,3), Set(5))),
			Label(Vector(Set(1,3), Set(5))) ~ Label(Vector(Set(1,2), Set(4)))
		)
		val g1 = SphereApproximation.subdivide(g)
		g1.nodes.size should equal(6)
		g1.edges.size should equal(12)
		g1.nodes should contain( Label(1) )
		g1.nodes should contain( Label( Vector(Set(1,2), Set(4))) )
		g1.nodes.exists { node ⇒
			node.layer == 3 && node.label(2).equals(Set(1,4))
		}
	}
}
