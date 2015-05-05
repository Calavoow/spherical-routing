import java.io.PrintWriter

import graph.{Dot, sphere, ring}
import instrumentation.Metric

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

object Main {
	def main(args: Array[String]) {
		val g = ring.Units.ring(0)

		var counter = 0
		Iterator.from(0).map { i ⇒
			println("Calculating ring")
			val subdiv = ring.Units.ring(i)
			writeToFile(s"ring_$counter", Dot.toDot(subdiv))
			counter += 1
			subdiv
		} take(4) foreach{ _ ⇒ }
	}

	def getNode(g: Graph[sphere.Units.Node, UnDiEdge], id: Int) : g.NodeT = {
		g.nodes.find(_.id == id).get
	}

	def instrumentPathCount(g: Graph[sphere.Units.Node ,UnDiEdge], iterateSubdivs: Iterator[Graph[sphere.Units.Node, UnDiEdge]]) = {
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

	def instrumentRandomCollisions(g: Graph[sphere.Units.Node, UnDiEdge], iterateSubdivs: Iterator[Graph[sphere.Units.Node, UnDiEdge]]) = {
		iterateSubdivs.map { graph ⇒
			Metric.randomCollisionCount(graph, g, concurrentPaths = 2, samples = 10000)
		}
	}

	def instrumentCollisionsAndConcurrent(g: Graph[sphere.Units.Node, UnDiEdge], iterateSubdivs: Iterator[Graph[sphere.Units.Node, UnDiEdge]]) = {
		iterateSubdivs.drop(2).map { graph ⇒
			(2 to 10).map { concurrentPaths ⇒
				Metric.randomCollisionCount(g = graph, g0 = g, concurrentPaths, 10000)
			}
		} foreach { collisions ⇒
			println(collisions.mkString(","))
		}
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
