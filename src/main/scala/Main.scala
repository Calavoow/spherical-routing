import java.io.PrintWriter

import graph.{Units, Dot, SphereApproximation}
import graph.Units.{Label, Node}

import scalax.collection.Graph
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
		val g = Units.icosahedron
		writeToFile("graph0.dot", Dot.toDot(g))
		val subdivided = SphereApproximation.subdivide(g)
		writeToFile("graph1.dot", Dot.toDot(subdivided))
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
