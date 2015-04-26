import java.io.PrintWriter

import graph._
import graph.Units._
import instrumentation.Metric

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

object Main {
	def main(args: Array[String]) {
		val g = Units.triangle

//		var counter = 1
		def iterateSubdivs = {
			Iterator.iterate(g) { graph ⇒
//				println("Calculating subdivision")
				SphereApproximation.subdivide(graph)
				//			writeToFile(s"subdiv_tri$counter", Dot.toDot(subdiv))
				//			counter += 1
				//			subdiv
			}
		}

//		val collisions = iterateSubdivs.map { graph ⇒
//			Metric.randomCollisionCount(graph, g, 10000)
//		} take(8) foreach { collisions ⇒
//			print("," + collisions)
//		}

		val graph = iterateSubdivs.drop(4).next()
		println(getNode(graph, 61))
		println(getNode(graph, 41))
		println(getNode(graph, 40))
		println(getNode(graph, 14))
		println(getNode(graph, 13))
		println(getNode(graph, 4))
		println(getNode(graph, 6))
		println(getNode(graph, 5))
	}

	def getNode(g: Graph[Units.Node, UnDiEdge], id: Int) : g.NodeT = {
		g.nodes.find(_.id == id).get
	}

	def instrumentPathCount(g: Graph[Node ,UnDiEdge], iterateSubdivs: Iterator[Graph[Node, UnDiEdge]]) = {
		val pathsPerLayerRouting = iterateSubdivs.map { graph ⇒
			Metric.countRoutingPaths(graph, g)
		}
		val pathsPerLayerShortest = iterateSubdivs.map { graph ⇒
			Metric.countShortestPaths(graph)
		}

		def mapsToString[K : Ordering,V](it: Iterator[Map[K,V]], takeCount: Int): String = {
			val csvPathCount = it.map(_.toSeq.sortBy(_._1).map(_._2).mkString(","))
			csvPathCount.take(takeCount).mkString("\n")
		}

		writeToFile("pathCountRouting_ico.csv", mapsToString(pathsPerLayerRouting, 5))
		writeToFile("pathCountingShortest.csv", mapsToString(pathsPerLayerShortest, 4))
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
