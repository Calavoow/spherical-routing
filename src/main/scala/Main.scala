import java.io.PrintWriter

import graph._

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

object Main {
	def main(args: Array[String]) {
		val g = Units.triangle

		var counter = 1
		val iterateSubdivs = Iterator.iterate(g) { graph ⇒
			println("Calculating subdivision")
			val subdiv = SphereApproximation.subdivide(graph)
			writeToFile(s"subdiv_tri$counter", Dot.toDot(subdiv))
			counter += 1
			subdiv
		}

//		val occurences = iterateSubdivs.map { graph ⇒
//			val paths = graph.nodes.toSeq.combinations(2).map {
//				case Seq(node1, node2) =>
////					Routing.route(graph, Units.icosahedron)(node1, node2)
//					node1.shortestPathTo(node2).get
//			}
//			val pathsThroughLayer = paths.map { path ⇒
//				val layers = path.nodes.toIterator.sliding(2).map {
//					case Seq(node1, node2) =>
//						Math.max(node1.layer, node2.layer)
//				}
//				layers.toSeq.groupBy(identity).mapValues(_.size)
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
//		writeToFile("occurences_shortest.csv", occurences.take(4).mkString("\n"))


		def getNode(g: Graph[Units.Node, UnDiEdge], id: Int) : g.NodeT = {
			g.nodes.find(_.id == id).get
		}
		val graph = iterateSubdivs.drop(4).next()
		val node1 = getNode(graph, 34)
		val node2 = getNode(graph, 30)
		println(node1)
		println(getNode(graph, 15))
		println(node2)
		val path = Routing.route(graph, g)(node1, node2)
		println(path.nodes)
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
