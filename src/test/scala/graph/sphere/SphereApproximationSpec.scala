package graph.sphere

import graph.Util.triangles
import graph.sphere.Units._
import org.scalatest.{FlatSpec, Matchers}

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class SphereApproximationSpec extends FlatSpec with Matchers {
	"Triangles" should "be correctly found in a triangular graph" in {
		val g = Units.triangle
		triangles(g) should equal(Set(
			Set(Label(1), Label(3), Label(2))
		))
	}

	it should "be correctly found in a moderate graph" in {
		val g = Graph[Node, UnDiEdge](
			Label(1) ~ Label(2),
			Label(2) ~ Label(3),
			Label(3) ~ Label(1),
			Label(2) ~ Label(4),
			Label(4) ~ Label(3),
			Label(1) ~ Label(5)
		)
		triangles(g) should equal(
			Set(
				Set(Label(1), Label(3), Label(2)),
				Set(Label(4), Label(3), Label(2))
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
		g1.nodes.find(_.label.last == Set(1,2)) should be('defined)
		g1.nodes.find(_.label.last == Set(2,3)) should be('defined)
		g1.nodes.find(_.label.last == Set(1,3)) should be('defined)
	}

	it should "always divide in the same way" in {
		val graphs = for(_ <- 1 to 100) yield {
			SphereApproximation.repeatedSubdivision(icosahedron).drop(3).next()
		}

		graphs.sliding(2).foreach {
			case Seq(g1, g2) => g1 should equal(g2)
		}
	}

	it should "subdivide a triangle of different layers" in {
		val g = Graph[Node, UnDiEdge](
			Label(1) ~ Label(Vector(Set(1,2), Set(4)),1),
			Label(1) ~ Label(Vector(Set(1,3), Set(5)),1),
			Label(Vector(Set(1,3), Set(5)), 1) ~ Label(Vector(Set(1,2), Set(4)), 1)
		)
		val g1 = SphereApproximation.subdivide(g)
		g1.nodes.size should equal(6)
		g1.edges.size should equal(12)
		g1.nodes should contain( Label(1) )
		g1.nodes should contain( Label( Vector(Set(1, 2), Set(4)), 1) )
		println(g1)
		// There must be a node between 1 and 4.
		assert(g1.nodes.exists { node ⇒
			node.layer == 2 && node.label.last.equals(Set(1, 4))
		})
	}

	it should "have unique IDs" in {
		val graphs = SphereApproximation.repeatedSubdivision(icosahedron)
		graphs.take(4).foreach { g ⇒
			val nodes = g.nodes.toVector.map(_.id)
			assert(nodes.distinct.size == nodes.size)
		}
	}

	"The labelling" should "have a maximum size of k+1 on the triangle" in {
		val gs = SphereApproximation.repeatedSubdivision(Units.triangle)
		gs.take(10).foreach { graph ⇒
			println("Tesing next subdivision.")
			graph.nodes.par.foreach{ node ⇒
				node.label.zipWithIndex.foreach{ case (set, index) ⇒
					assert(set.size <= index + 1, s"Node had label larger than ${index + 1}: $node")
				}
			}
		}
	}

	it should "have a maximum size of k+1 on the icosahedron" in {
		val gs = SphereApproximation.repeatedSubdivision(Units.icosahedron)
		gs.take(8).foreach { graph ⇒
			println("Tesing next subdivision.")
			graph.nodes.par.foreach{ node ⇒
				node.label.zipWithIndex.foreach{ case (set, index) ⇒
					assert(set.size <= index + 1, s"Node had label larger than ${index + 1}: $node")
				}
			}
		}
	}
}
