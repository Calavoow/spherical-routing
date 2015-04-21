import java.io.PrintWriter

import graph._
import graph.Units.{Label, Node}

import scala.concurrent.Future
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.io.dot._

object Main {
	def main(args: Array[String]) {
		val g = Units.triangle
		println(g)

		var counter = 1
		val iterateSubdivs = Iterator.iterate(g) { graph ⇒
			println("Calculating subdivision")
			val subdiv = SphereApproximation.subdivide(graph)
			writeToFile(s"subdiv_tri$counter", Dot.toDot(subdiv))
			counter += 1
			subdiv
		}

//		val occurences = iterateSubdivs.map { graph ⇒
//			val paths = Paths.all(graph)
//			val pathsThroughLayer = paths.map { path ⇒
//				path.edges.groupBy { edge ⇒
//					edge.nodes.map(_.parentalLabel.size).max
//				} mapValues(_.size)
//			}
//			val nrPathsPerLayer = pathsThroughLayer.reduceLeft { (accum, nrPathsPerLayer) ⇒
//				accum ++ nrPathsPerLayer.map {
//					case (k,v) ⇒ k → (v + accum.getOrElse(k,0))
//				}
//			}
//			nrPathsPerLayer.toSeq.sortBy {
//				case (layer, _) ⇒ layer
//			}.map {
//				case (layer, occurrences) ⇒ s"$occurrences"
//			}.mkString(",")
//		}
//		writeToFile("occurences.csv", occurences.mkString("\n"))

		val routeGraph = iterateSubdivs.drop(4).next()
//		val from = routeGraph.get(Label(1))
//		val to = routeGraph.get(Label(2))
//		val path = Routing.route(routeGraph, Units.triangle)(from, to)
//		println(path)
		def getNode(id: Int) = {
			routeGraph.nodes.find(_.id == id).get
		}
		println(getNode(57))
		println(getNode(38))
		println(getNode(34))
		println(getNode(9))
		println(getNode(10))
		println(getNode(5))
		println(getNode(4))

//		val g = Units.icosahedron
//		writeToFile("graph0.dot", Dot.toDot(g))
//		val subdivided = SphereApproximation.subdivide(g)
//		writeToFile("graph1.dot", Dot.toDot(subdivided))

//		val route = Routing.route(g)(g.get(Label(0,1)), g.get(Label(0,4)))
//		println(route)
	}

	def writeToFile(fileName: String, content: String) {
		val writer = new PrintWriter(fileName)
		try {
			writer.print(content)
			writer.flush()
		} finally {
			writer.close()
		}
	}
}
