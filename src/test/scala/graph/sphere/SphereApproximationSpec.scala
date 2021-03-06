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
			Set(SphereNode(0), SphereNode(2), SphereNode(1))
		))
	}

	it should "be correctly found in a moderate graph" in {
		val g = Graph[Node, UnDiEdge](
			SphereNode(1) ~ SphereNode(2),
			SphereNode(2) ~ SphereNode(3),
			SphereNode(3) ~ SphereNode(1),
			SphereNode(2) ~ SphereNode(4),
			SphereNode(4) ~ SphereNode(3),
			SphereNode(1) ~ SphereNode(5)
		)
		triangles(g) should equal(
			Set(
				Set(SphereNode(1), SphereNode(3), SphereNode(2)),
				Set(SphereNode(4), SphereNode(3), SphereNode(2))
			)
		)
	}

	"Subdivision" should "subdivide a triangle" in {
		val g = Units.triangle
		val g1 = SphereApproximation.subdivide(g)
		g1.nodes.size should equal(6)
		g1.edges.size should equal(12)
		g1.nodes should contain( SphereNode(0) )
		g1.nodes should contain( SphereNode(1) )
		g1.nodes should contain( SphereNode(2) )
		g1.nodes.foreach(println)
		g1.nodes.exists( node => SphereNode.parents(g1)(node).map(_.id).toSet == Set(0,1) ) should be(true)
		g1.nodes.exists( node => SphereNode.parents(g1)(node).map(_.id).toSet == Set(1,2) ) should be(true)
		g1.nodes.exists( node => SphereNode.parents(g1)(node).map(_.id).toSet == Set(0,2) ) should be(true)
	}

	it should "always divide in the same way" in {
		val graphs = Iterator.continually(SphereApproximation.repeatedSubdivision(icosahedron).drop(4).next())

		graphs.sliding(2).take(250).foreach {
			case Seq(g1, g2) => assert(g1.equals(g2), s"Graph 1:$g1\nDoes not equal Graph 2:$g2")
		}
	}

	it should "subdivide a triangle of different layers" in {
		val node4 = new SphereNode(4, 1)
		val node5 = new SphereNode(5, 1)
		val g = Graph[Node, UnDiEdge](
			SphereNode(1) ~ node4,
			SphereNode(1) ~ node5,
			node4 ~ node5
		)
		val g1 = SphereApproximation.subdivide(g)
		g1.nodes.size should equal(6)
		g1.edges.size should equal(12)
		g1.nodes should contain( SphereNode(1) )
		g1.nodes should contain( new SphereNode(4,1) )
		println(g1)
		// There must be a node between 1 and 4, but only have 1 as parent.
		assert(g1.nodes.exists { node ⇒
			node.layer == 2 && SphereNode.parents(g1)(node).exists(_.id == 1)
		})
	}

	it should "have unique IDs" in {
		val graphs = SphereApproximation.repeatedSubdivision(icosahedron)
		graphs.take(4).foreach { g ⇒
			val nodes = g.nodes.toVector.map(_.id)
			assert(nodes.distinct.size == nodes.size)
		}
	}
	/*
	"The labelling" should "have a min size 1, and max size of 3 on the triangle" in {
		val gs = SphereApproximation.repeatedSubdivision(Units.triangle)
		gs.take(7).foreach { graph ⇒
			println("Testing next subdivision.")
			graph.nodes.par.foreach{ node ⇒
				node.label.foreach { labelEl =>
					assert(labelEl.size >= 1, s"Node had label equal to 0 size: $node")
					assert(labelEl.size <= 3, s"Node had too large label: $node")
				}
			}
		}
	}

	it should "have a min size 1 and max size of 3 on the icosahedron" in {
		val gs = SphereApproximation.repeatedSubdivision(Units.icosahedron)
		gs.take(6).foreach { graph ⇒
			println("Testing next subdivision.")
			graph.nodes.par.foreach{ node ⇒
				node.label.foreach { labelEl =>
					assert(labelEl.size >= 1, s"Node had label equal to 0 size: $node")
					assert(labelEl.size <= 3, s"Node had too large label: $node")
				}
			}
		}
	}

	it should "not have 2 vertices after 2 vertices in the label" in {
		val gs = SphereApproximation.repeatedSubdivision(Units.icosahedron)
		gs.take(6).foreach { graph ⇒
			println("Testing next subdivision.")
			graph.nodes.par.foreach{ node ⇒
				if(node.label.size > 1) {
					node.label.sliding(2).foreach {
						case Seq(prevLabel, curLabel) =>
							if(prevLabel.size == 2) {
								assert(curLabel.size != 2, s"Two vertices in label after two vertices for node: $node")
							}
					}
				}
			}
		}
	}
	*/
}
