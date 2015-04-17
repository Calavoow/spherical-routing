package graph

import graph.Units._
import org.scalatest.{Matchers, FlatSpec}

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

class RoutingSpec extends FlatSpec with Matchers {
	"Route" should "find a one-hop path" in {
		val g = Graph[Node, UnDiEdge](
			Label(Vector(Set(1))) ~ Label(Vector(Set(2))),
			Label(Vector(Set(2))) ~ Label(Vector(Set(3))),
			Label(Vector(Set(3))) ~ Label(Vector(Set(1))),
			Label(Vector(Set(2))) ~ Label(Vector(Set(4))),
			Label(Vector(Set(4))) ~ Label(Vector(Set(3))),
			Label(Vector(Set(1))) ~ Label(Vector(Set(5)))
		)

		val route = Routing.route(g,g)(g.get(Label(Vector(Set(1)))), g.get(Label(Vector(Set(5)))))

		route.nodes.toList should be(List(Label(Vector(Set(1))), Label(Vector(Set(5)))))
		route.edges.toList should be(List(Label(Vector(Set(1))) ~ Label(Vector(Set(5)))))
	}

	it should "find a two-hop-path" in {
		val g = Graph[Node, UnDiEdge](
			Label(Vector(Set(1))) ~ Label(Vector(Set(2))),
			Label(Vector(Set(2))) ~ Label(Vector(Set(3))),
			Label(Vector(Set(3))) ~ Label(Vector(Set(1))),
			Label(Vector(Set(2))) ~ Label(Vector(Set(4))),
			Label(Vector(Set(4))) ~ Label(Vector(Set(3))),
			Label(Vector(Set(1))) ~ Label(Vector(Set(5)))
		)

		val route = Routing.route(g,g)(g.get(Label(Vector(Set(1)))), g.get(Label(Vector(Set(4)))))

		route.nodes.toList should (equal (List(Label(Vector(Set(1))), Label(Vector(Set(3))), Label(Vector(Set(4)))))
			or equal (List(Label(Vector(Set(1))), Label(Vector(Set(2))), Label(Vector(Set(4))))))
		route.edges.toList should (equal (List(Label(Vector(Set(1))) ~ Label(Vector(Set(3))), Label(Vector(Set(3))) ~ Label(Vector(Set(4)))))
			or equal (List(Label(Vector(Set(1))) ~ Label(Vector(Set(2))), Label(Vector(Set(2))) ~ Label(Vector(Set(4))))))
	}

	it should "find an m+1 path on the face for specific nodes" in {
		val g = SphereApproximation.repeatedSubdivision(triangle).drop(3).next()
		val node1 = g.get(Label(IndexedSeq(Set(2,3,1), Set(4,6), Set(9), Set(63))))
		val node2 = g.get(Label(IndexedSeq(Set(3,1,2), Set(5,6), Set(12))))
		val shortPath = node1.shortestPathTo(node2).get
		val route = Routing.route(g,triangle)(node1,node2)
		(shortPath.edges.size + 1) should be >= route.edges.size
	}

	it should "find an m+1 path on the face 4 for specific nodes" in {
		val g = SphereApproximation.repeatedSubdivision(triangle).drop(3).next()
		val node1 = g.get(Label(IndexedSeq(Set(2,3,1), Set(4,6), Set(9), Set(63))))
		val node2 = g.get(Label(IndexedSeq(Set(3,1), Set(5))))
		val shortPath = node1.shortestPathTo(node2).get
		val route = Routing.route(g,triangle)(node1,node2)
		(shortPath.edges.size + 1) should be >= route.edges.size
	}

	it should "find an m+1 path on the face" in {
		// Make the three times subdivision graph.
		val graphs = SphereApproximation.repeatedSubdivision(triangle)
		graphs.take(4).foreach(g => atMostM1Path(g, triangle))
	}

	it should "find an m+1 path upto 5 divisions" in {
		val graphs = SphereApproximation.repeatedSubdivision(icosahedron)
		graphs.take(3).foreach(g => atMostM1Path(g, icosahedron))
	}

	def atMostM1Path(g : Graph[Node, UnDiEdge], g0: Graph[Node, UnDiEdge]): Unit = {
		g.nodes.toSeq.combinations(2).toIterable.par.foreach {
			case Seq(node1, node2) =>
				val shortestPath = node1.shortestPathTo(node2).get
				val route = Routing.route(g, g0)(node1, node2)
				assert(shortestPath.edges.size + 1 >= route.edges.size, s"Shortestpath + 1 was longer than route for nodes ($node1, $node2).\n${shortestPath.nodes}\n${route.nodes}")
//				(shortestPath.edges.size+1) should be >= route.edges.size
		}
	}
}
