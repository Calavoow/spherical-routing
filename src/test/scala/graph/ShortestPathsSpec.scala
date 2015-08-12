package graph

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class ShortestPathsSpec extends FlatSpec with Matchers {
	"ShortestPaths" should "give the shortest path up to 4 subdivisions" in {
		val ico = sphere.SphereApproximation.repeatedSubdivision(sphere.Units.icosahedron)
		ico.take(4).foreach { g =>
			val shortestPaths = ShortestPaths.allDistances(g)
			g.nodes.toSeq.combinations(2).foreach {
				case Seq(node1,node2) =>
					val shortestPath = node1.shortestPathTo(node2).get
					assert(shortestPath.edges.size == shortestPaths(node1,node2))
					assert(shortestPath.edges.size == shortestPaths(node2,node1))
			}
		}
	}
}
