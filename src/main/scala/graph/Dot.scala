package graph

import graph.Units._

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._

object Dot {
	def toDot(g: Graph[Node,UnDiEdge]) = {
		val dotRoot = DotRootGraph(
			directed  = false,
			id        = Some("MyDot")
		)

		def edgeTransformer(innerEdge: Graph[Node,UnDiEdge]#EdgeT):
		Option[(DotGraph,DotEdgeStmt)] = innerEdge.edge match {
			case UnDiEdge(source, target) =>
				Some((dotRoot,
					DotEdgeStmt(source.toString,
						target.toString
					)
				))
		}

		new Export(g).toDot(dotRoot, edgeTransformer)
	}

}
