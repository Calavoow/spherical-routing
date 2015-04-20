import java.io.PrintWriter

import graph._
import graph.Units.{Label, Node}

import scala.concurrent.Future
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.io.dot._

object Main {
	def main(args: Array[String]) {
		val g = Units.icosahedron

		val iterateSubdivs = Iterator.iterate(g) { graph ⇒
			println("Calculating subdivision")
			SphereApproximation.subdivide(graph)
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

		val routeGraph = iterateSubdivs.drop(6).next()
		val from = routeGraph.get(Label(1))
		val to = routeGraph.get(Label(2))
		val path = Routing.route(routeGraph, Units.icosahedron)(from, to)
		println(path)

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
