package graph

import graph.Units._

import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Util {
	implicit class RichGraph[N, E[X] <: EdgeLikeIn[X]](g: Graph[N,E]) {
		def diameter = {
			val nodes = g.nodes
			(for (node ← nodes;
			                  toNode ← nodes) yield {
				val path = node.shortestPathTo(toNode)
				path.map(p ⇒ p.edges.size).getOrElse(Int.MaxValue)
			}).max
		}
	}

	/**
	 * Find the set of triangles, where each triangles is represented by a set of 3.
	 *
	 * The scala.collection.Set is required because no immutable.Set[A] is returned by the for.
	 * @param g The graph to look into
	 * @return The set of triangles
	 */
	def triangles(g: Graph[Node, UnDiEdge]) : scala.collection.Set[Set[g.NodeT]] = {
		for(node : g.NodeT ← g.nodes;

            // The subgraph containing neighbors of node and their edges
		    subGraph = g.filter(g.having(_.diSuccessors.contains(node)));

		    // For every node in the subgraph, find all adjacent nodes and create a triangle.
			adjacentNode : subGraph.NodeT ← subGraph.nodes;
			triangleNode : subGraph.NodeT ← adjacentNode.diSuccessors
		) yield {
			// A triangle is represented by a Set of size 3.
			// This way triangles can easily be made distinct.
			Set[g.NodeT](node,g.get(adjacentNode),g.get(triangleNode))
		}
	}
}
