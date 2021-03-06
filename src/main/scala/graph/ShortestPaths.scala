package graph

import graph.Util.ID

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.GraphBase

import scala.collection.mutable
import scala.language.implicitConversions

object ShortestPaths {
	/**
	 * Calaculate the distances between all nodes in the graph.
	 *
	 * We use the Floyd-Warshal algorithm for this, which is faster than calculating each path individually.
	 */
	def allDistances[T](g: Graph[T,UnDiEdge])(implicit ev: ID[T]) : Map[(g.NodeT,g.NodeT), Int]= {

//		import InfIntegerIsNumeric._

		val vSize = g.nodes.size
		val dist = mutable.IndexedSeq.fill(vSize,vSize)(vSize+1) // This is larger than any path.
		for(v <- 0 until vSize) {
			dist(v)(v) = 0
		}

		for(edge <- g.edges) {
			val Seq(u,v) = edge.nodeSeq.map(inner => ev.id(inner))
			dist(u)(v) = 1
			dist(v)(u) = 1
		}

		var highestK = 0
		for(
			k <- 0 until vSize;
			i <- 0 until vSize;
			j <- 0 until vSize
		) {
			if(dist(i)(j) > dist(i)(k) + dist(k)(j)) {
				dist(i)(j) = dist(i)(k) + dist(k)(j)
				dist(j)(i) = dist(i)(k) + dist(k)(j)
			}

			if(k > highestK && (k % 100) == 0) {
				println(s"$k / $vSize")
				highestK = k
			}
		}

		// Create a nodeMap because looking up the ID is O(n)
		val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[T]].id(node))
		val indexed = dist.map(_.zipWithIndex).zipWithIndex
		(for((entry, idx1) <- indexed;
		    (distance, idx2) <- entry) yield {
			val node1 = nodeMap(idx1)
			val node2 = nodeMap(idx2)
			(node1,node2) -> distance
		}).toMap
	}
}
