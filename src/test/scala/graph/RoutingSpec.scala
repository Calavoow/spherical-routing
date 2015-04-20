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
		val node1 = g.get(Label(IndexedSeq(Set(2,3,1), Set(4,6), Set(9,4), Set(54))))
		val node2 = g.get(Label(IndexedSeq(Set(3,1,2), Set(5,6), Set(12))))
		val shortestPath = node1.shortestPathTo(node2).get
		val route = Routing.route(g,triangle)(node1,node2)
		assert(shortestPath.edges.size + 1 >= route.edges.size, s"Shortestpath + 1 was longer than route for nodes ($node1, $node2).\n${shortestPath.nodes}\n${route.nodes}")
	}

	it should "find an m+1 path on the face 4 for specific nodes" in {
		val g = SphereApproximation.repeatedSubdivision(triangle).drop(3).next()
		val node1 = g.get(Label(IndexedSeq(Set(2,3,1), Set(4,6), Set(9,4), Set(63))))
		val node2 = g.get(Label(IndexedSeq(Set(3,1), Set(5))))
		val shortPath = node1.shortestPathTo(node2).get
		val route = Routing.route(g,triangle)(node1,node2)
		(shortPath.edges.size + 1) should be >= route.edges.size
	}

	it should "find the same path 100 times" in {
		val paths = for(_ <- 1 to 100) yield {
			val g = SphereApproximation.repeatedSubdivision(triangle).drop(3).next()

			val node1 = g.get(Label(IndexedSeq(Set(2, 3), Set(4), Set(8), Set(29))))
			val node2 = g.get(Label(IndexedSeq(Set(2, 3), Set(16))))
			val shortestPath = node1.shortestPathTo(node2).get
			val route = Routing.route(g, triangle)(node1, node2)
			(shortestPath, route)
		}

		paths.sliding(2).foreach {
			case Seq((shortestPath1, route1), (shortestPath2, route2)) =>
				shortestPath1.edges.size should equal(shortestPath2.edges.size)
				assert(route1.edges.size == route2.edges.size, s"Routes were not of equal length.\n $route1\n $route2.")
		}
		println(paths.head._2.edges.size)
	}

	it should "find an m+1 path on the face 4 for specific nodes 2" in {
		val g = SphereApproximation.repeatedSubdivision(triangle).drop(3).next()
		val parG = Iterator.iterate(triangle)(SphereApproximation.parSubdivide).drop(3).next()

		g === parG

		def routes(g : Graph[Node, UnDiEdge]) : (g.Path, g.Path) = {
			val node1 = g.get(Label(IndexedSeq(Set(2,3), Set(4), Set(8), Set(29))))
			val node2 = g.get(Label(IndexedSeq(Set(2,3), Set(16))))
			val shortestPath = node1.shortestPathTo(node2).get
			val route = Routing.route(g,triangle)(node1,node2)

			(shortestPath, route)
		}
		val (gShort, gRoute) = routes(g)
		val (parShort, parRoute) = routes(parG)
		gShort === parShort
		gRoute === parRoute

		assert(parShort.edges.size + 1 >= parRoute.edges.size, s"Parallel Shortestpath + 1 was longer than route.")
		assert(gShort.edges.size + 1 >= gRoute.edges.size, s"Shortestpath + 1 was longer than route.")
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
		g.nodes.toSeq.combinations(2).toIterable.foreach {
			case Seq(node1, node2) =>
				val shortestPath = node1.shortestPathTo(node2).get
				val route = Routing.route(g, g0)(node1, node2)
				assert(shortestPath.edges.size + 1 >= route.edges.size, s"Shortestpath + 1 was longer than route for nodes ($node1, $node2).\n${shortestPath.nodes}\n${route.nodes}")
//				(shortestPath.edges.size+1) should be >= route.edges.size
		}
	}
}
