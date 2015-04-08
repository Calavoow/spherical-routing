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

		val route = Routing.route(g)(g.get(Label(Vector(Set(1)))), g.get(Label(Vector(Set(5)))))

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

		val route = Routing.route(g)(g.get(Label(Vector(Set(1)))), g.get(Label(Vector(Set(4)))))

		route.nodes.toList should (equal (List(Label(Vector(Set(1))), Label(Vector(Set(3))), Label(Vector(Set(4)))))
			or equal (List(Label(Vector(Set(1))), Label(Vector(Set(2))), Label(Vector(Set(4))))))
		route.edges.toList should (equal (List(Label(Vector(Set(1))) ~ Label(Vector(Set(3))), Label(Vector(Set(3))) ~ Label(Vector(Set(4)))))
			or equal (List(Label(Vector(Set(1))) ~ Label(Vector(Set(2))), Label(Vector(Set(2))) ~ Label(Vector(Set(4))))))
	}
}
