import java.io.{FileWriter, PrintWriter}

import graph.ring.Units.{Ring,LayeredNode}
import graph.sphere.Units.Sphere
import graph.sphere.Units.Label.LayeredLabel
import graph.{Util, Dot, sphere, ring}
import instrumentation.Metric
import instrumentation.Metric.Router

import scala.io.StdIn
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

object Main {
	def main(args: Array[String]) {
		val (ringFile, sphereFile) = if(args.length == 2) {
			println(s"(Over)writing to files ${args(0)} and ${args(1)}, are you sure? [y/N]")
			val answer = StdIn.readLine()
			if (answer == "y") {
				(new FileWriter(args(0)), new FileWriter(args(1)))
			} else {
				println("Answer was not 'y', exiting...")
				return
			}
		} else {
			println(s"No output files given.")
			return
		}
		// Write headers to files.
		ringFile.write("nodes,concurrentpaths\n")
		ringFile.flush()
		sphereFile.write("nodes, concurrentpaths\n")
		sphereFile.flush()

		val g0 = sphere.Units.icosahedron
		val samples = 10000
		val ancestorPathMap = Util.allShortestPaths(g0)


		val rings : Iterator[Ring] = Iterator.from(0).map { i ⇒
			ring.Units.ring(i)
//			writeToFile(s"ring_$counter.dot", Dot.toDot(subdiv))
//			counter += 1
//			subdiv
		}
		val spheres : Iterator[Sphere] = sphere.SphereApproximation.repeatedSubdivision(g0)
		// Ring must have size at least 20, so drop 3 for size 32.
		// Sphere must have size at least 20, so drop 1 for size 42
		// Ring grows with 2^k, sphere with 4^k. So for each sphere, drop the second ring.
		val graphs : Iterator[(Ring, Sphere)] = rings.drop(3).grouped(2).map(_.head).zip(spheres.drop(1))
		try {
			graphs.foreach {
				case (ringG, sphereG) ⇒
					val ringNodes = ringG.nodes.size
					val sphereNodes = sphereG.nodes.size
					println(s"Next iteration, ring ($ringNodes), sphere ($sphereNodes})")
					// Iterate through the number of concurrent paths.
					(2 to 10).foreach { concurrentPaths ⇒
						println(s"Concurrent paths: $concurrentPaths")
						val ringCollisions = Metric.randomCollisionCount(g = ringG, concurrentPaths = concurrentPaths, samples)(ring.Routing)
							.map(_.getOrElse(-1))
						// Probably faster to concat in memory (`samples` integers) and then write at once.
						ringFile.write(s"$ringNodes,$concurrentPaths,${ringCollisions.mkString(",")}\n")
						ringFile.flush()
						val sphereCollisions = Metric
							.randomCollisionCount(g = sphereG,
						        concurrentPaths = concurrentPaths,
						        samples = samples) (sphere.Routing.sphereRouter(g0)(ancestorPathMap))
							.map(_.getOrElse(-1))
						sphereFile.write(s"$sphereNodes,$concurrentPaths,${sphereCollisions.mkString(",")}\n")
						sphereFile.flush()
					}
			}
		} finally {
			ringFile.close()
			sphereFile.close()
		}
	}

	def getNode(g: Graph[sphere.Units.Node, UnDiEdge], id: Int) : g.NodeT = {
		g.nodes.find(_.id == id).get
	}
	/*
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
			Metric.randomCollisionCount(graph, concurrentPaths = 2, samples = 10000)(sphere.Routing.SphereRouter(g))
		}
	}

	def instrumentCollisionsAndConcurrent(g: Graph[sphere.Units.Node, UnDiEdge], iterateSubdivs: Iterator[Graph[sphere.Units.Node, UnDiEdge]]) = {
		iterateSubdivs.drop(2).map { graph ⇒
			(2 to 10).map { concurrentPaths ⇒
				Metric.randomCollisionCount(g = graph, concurrentPaths, 10000)(sphere.Routing.SphereRouter(g))
			}
		} foreach { collisions ⇒
			println(collisions.mkString(","))
		}
	}
	*/

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
