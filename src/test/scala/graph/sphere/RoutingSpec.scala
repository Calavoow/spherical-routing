package graph.sphere

import graph.Util
import graph.Util.ID
import graph.sphere.Units._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class RoutingSpec extends FlatSpec with Matchers {
	/*
	"Labelroute" should "route from a child to a parent" in {
		val g = SphereApproximation.repeatedSubdivision(triangle).drop(3).next()
		val child = g.nodes.find(_.id == 18).get
		val parent = g.nodes.find(_.id == 2).get
		val path = Routing.labelRoute(g)(child = child, parent = parent)
		assert(path.nodes.size == 3, s"Path was different than expected.\n $path")
	}
	*/

	"Route" should "find a one-hop path" in {
		val g = Graph[Node, UnDiEdge](
			Label(1) ~ Label(2),
			Label(2) ~ Label(3),
			Label(3) ~ Label(1),
			Label(2) ~ Label(4),
			Label(4) ~ Label(3),
			Label(1) ~ Label(5)
		)

		val pathMap = Util.allShortestPaths(g)
		val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[Units.Node]].id(node))

		val router = Routing.sphereRouter(g)(pathMap)
		val route = router.route(g, g.nodes.size)(g.get(Label(1)), g.get(Label(5)), nodeMap)

		route.nodes.toList should be(List(Label(1), Label(5)))
		route.edges.toList should be(List(Label(1) ~ Label(5)))
	}

	it should "find a two-hop-path" in {
		val g = Graph[Node, UnDiEdge](
			Label(1) ~ Label(2),
			Label(2) ~ Label(3),
			Label(3) ~ Label(1),
			Label(2) ~ Label(4),
			Label(4) ~ Label(3),
			Label(1) ~ Label(5)
		)

		val pathMap = Util.allShortestPaths(g)
		val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[Units.Node]].id(node))

		val router = Routing.sphereRouter(g)(pathMap)
		val route = router.route(g, g.nodes.size)(g.get(Label(1)), g.get(Label(4)), nodeMap)

		route.nodes.toList should (equal (List(Label(1), Label(3), Label(4)))
			or equal (List(Label(1), Label(2), Label(4))))
		route.edges.toList should (equal (List(Label(1) ~ Label(3), Label(3) ~ Label(4)))
			or equal (List(Label(1) ~ Label(2), Label(2) ~ Label(4))))
	}

	it should "find the same path 100 times" in {
		val pathMap = Util.allShortestPaths(triangle)
		val router = Routing.sphereRouter(triangle)(pathMap)
		val paths = for(_ <- 1 to 100) yield {
			val g = SphereApproximation.repeatedSubdivision(triangle).drop(3).next()
			val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[Units.Node]].id(node))

			val node1 = g.nodes.find(_.id == 29).get
			val node2 = g.nodes.find(_.id == 16).get
			val shortestPath = node1.shortestPathTo(node2).get
			val route = router.route(g, g.nodes.size)(node1, node2, nodeMap)
			(shortestPath, route)
		}

		paths.sliding(2).foreach {
			case Seq((shortestPath1, route1), (shortestPath2, route2)) =>
				shortestPath1.edges.size should equal(shortestPath2.edges.size)
				assert(route1.edges.size == route2.edges.size, s"Routes were not of equal length.\n $route1\n $route2.")
		}
	}

	it should "find a shortest path on the face upto 4 dvisions" in {
		// Make the three t, nodeMapimes subdivision graph.
		val graphs = SphereApproximation.repeatedSubdivision(triangle)
		graphs.take(4).foreach(g => atMostM1Path(g, triangle))
	}

	it should "find a shortest path upto 3 divisions" in {
		val graphs = SphereApproximation.repeatedSubdivision(icosahedron)
		graphs.take(3).foreach(g => atMostM1Path(g, icosahedron))
	}

	it should "find an m+1 path in 6 divisions" in {
		val graphs = SphereApproximation.repeatedSubdivision(icosahedron)
		graphs.drop(5).take(1).foreach(g => atMostM1Path(g, icosahedron))
	}

	it should "find an m+1 path on the face upto 8 division with random sampling" in {
		val graphs = SphereApproximation.repeatedSubdivision(triangle)
		val pathMap = Util.allShortestPaths(triangle)
		val router = Routing.sphereRouter(triangle)(pathMap)
		Random.setSeed(System.currentTimeMillis())
		graphs.drop(4).take(3).foreach { g =>
			val nrNodes = g.nodes.size
			val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[Units.Node]].id(node))

			Random.shuffle(g.nodes.toSeq.combinations(2)).take(1000).foreach {
				case Seq(node1, node2) =>
					val shortestPath = node1.shortestPathTo(node2).get
					val route = router.route(g, nrNodes)(node1, node2, nodeMap)
					assert(shortestPath.edges.size + 1 >= route.edges.size, s"Shortestpath + 1 was longer than route for nodes ($node1, $node2).\n${shortestPath.nodes}\n${route.nodes}")
			}
		}
	}

	def atMostM1Path(g : Graph[Node, UnDiEdge], g0: Graph[Node, UnDiEdge]): Unit = {
		val combinations = Util.binomCoef(BigInt(g.nodes.size), BigInt(2))
		val ancestorPathMap = Util.allShortestPaths(g0)
		val router = Routing.sphereRouter(g0)(ancestorPathMap)

		var counter = 0L

		val nrNodes = g.nodes.size
		val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[Units.Node]].id(node))

		g.nodes.toSeq.combinations(2).foreach {
			case Seq(node1, node2) =>
				val shortestPath = node1.shortestPathTo(node2).get
				val route = router.route(g, nrNodes)(node1, node2, nodeMap)
				assert(route.nodes.head == node1)
				assert(route.nodes.last == node2)
				assert(shortestPath.edges.size == route.edges.size, s"Shortestpath was unequal than shortest route for nodes ($node1, $node2).\n${shortestPath.nodes}\n${route.nodes}")
				if((counter % 10000L) == 0) println(s"$counter / $combinations")
				counter += 1
		}
	}
}
