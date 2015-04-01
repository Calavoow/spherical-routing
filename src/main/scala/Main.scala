import java.io.PrintWriter

import graph._
import graph.Units.{Label, Node}

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.io.dot._
import implicits._

object Main {
	def main(args: Array[String]) {
//		val g = Graph[Node, UnDiEdge](
//			Label(0,1) ~ Label(0,2),
//			Label(0,2) ~ Label(0,3),
//			Label(0,3) ~ Label(0,1),
//			Label(0,2) ~ Label(0,4),
//			Label(0,4) ~ Label(0,3),
//			Label(0,1) ~ Label(0,5)
//		)
//		val g = Units.icosahedron
		val g = Graph[Node, UnDiEdge] (
			Label(0,1) ~ Label(0,2),
			Label(0,2) ~ Label(0,3),
			Label(0,3) ~ Label(0,1)
		)
 		val g2 = SphereApproximation.subdivide(g)
		println(g2.toString)
		writeToFile("graph_tri1.dot", Dot.toDot(g2))


		val streamApprox = Stream.iterate(g) { graph ⇒
			println("next calculated")
			SphereApproximation.subdivide(graph)
		} take 5

		streamApprox.foreach { graph ⇒
			val paths = Paths.all(graph)
			val pathsThroughLayer = paths.map { path ⇒
				path.edges.groupBy { edge ⇒
					edge.nodes.map(_.level).max
				} mapValues(_.size)
			}
			val nrPathsPerLayer = pathsThroughLayer.reduce { (accum, nrPathsPerLayer) ⇒
				accum ++ nrPathsPerLayer.map {
					case (k,v) ⇒ k → (v + accum.getOrElse(k,0))
				}
			}
			println(nrPathsPerLayer.toSeq.sortBy {
				case (layer, _) ⇒ layer
			}.map {
				case (layer, occurrences) ⇒ s"layer: $layer, occurrences: $occurrences"
			}.mkString("\n"))
		}

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
