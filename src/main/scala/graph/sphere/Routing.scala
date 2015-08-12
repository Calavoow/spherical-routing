package graph.sphere

import graph.Util
import instrumentation.Metric.Router

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.immutable.Graph
import Units._

object Routing {
	def path(g: Sphere, g0: Sphere)(alpha: g.NodeT, beta: g.NodeT, ancestorMap: Map[(Int, Int), g0.Path], nodeMap: IndexedSeq[g.NodeT]) : g.Path = {
		pathRecursive(g, g0)(alpha, beta, List.empty, List.empty, ancestorMap, nodeMap)
	}

	@tailrec
	def pathRecursive(g: Sphere, g0: Sphere)(alpha: g.NodeT, beta: g.NodeT, pathA: List[g.NodeT], pathB: List[g.NodeT],
		ancestorMap: Map[(Int, Int), g0.Path], nodeMap: IndexedSeq[g.NodeT]) : g.Path = {
		val p6 = path6(g, g0)(alpha, beta, ancestorMap, nodeMap)
		p6 match {
			case Some(pab) =>
				val joinedPaths = Util.joinPaths(g)((alpha :: pathA).reverse, pab.nodes, beta :: pathB)
				joinedPaths
			case None =>
				def stepDown(v : g.NodeT) : g.NodeT = {
					val labelSeq = v.label(1).toIndexedSeq
					val vNewID = labelSeq(Random.nextInt(labelSeq.size))
					// val vNewID = v.label(1).head
					nodeMap(vNewID)
				}
				if(beta.layer > alpha.layer) {
					pathRecursive(g, g0)(alpha, stepDown(beta), pathA, beta :: pathB, ancestorMap, nodeMap)
				} else {
					pathRecursive(g, g0)(stepDown(alpha), beta, alpha :: pathA, pathB, ancestorMap, nodeMap)
				}
		}
	}

	def path6(g: Sphere, g0: Sphere)(alpha : g.NodeT, beta: g.NodeT, ancestorMap: Map[(Int, Int), g0.Path], nodeMap: IndexedSeq[g.NodeT]) : Option[g.Path] = {
		case class Breadcrumb(node : g.NodeT, breadcrumbs: List[g.NodeT] = List.empty) {
			def :: (newHead: g.NodeT) : Breadcrumb = Breadcrumb(newHead, node :: breadcrumbs)

			/**
			 * Provide methods so that breadcrumbs are put into a set like the ID of the [[node]] field.
			 **/
			override def hashCode(): Int = node.id.hashCode()
			override def equals(obj: scala.Any): Boolean = obj match {
				case b: Breadcrumb => b.node.id == node.id
				case _ => false
			}
		}

		def p(breadcrumbs : Set[Breadcrumb]) : Set[Breadcrumb] = {
			(for(breadcrumb <- breadcrumbs) yield {
				breadcrumb.node.parents.map { ps =>
					val p1 = nodeMap(ps._1.id) :: breadcrumb
					val p2 = nodeMap(ps._2.id) :: breadcrumb
					Set(p1, p2)
				}.getOrElse(Set.empty)
			}).flatten
		}

		def N(breadcrumbs : Set[Breadcrumb]) : Seq[Breadcrumb] = {
			// Start with shortest breadcrumbs first.
			val sorted = breadcrumbs.toSeq.sortBy(_.breadcrumbs.size)
			for(
				breadcrumb <- sorted;
				neighNode <- breadcrumb.node.neighbors
			) yield {
				neighNode :: breadcrumb
			}
		}

		val bcA = Set(Breadcrumb(alpha))
		val bcB = Set(Breadcrumb(beta))

		/**
		 * Note: We rely on the fact that on equality of Breadcrumbs, the first in the set is kept in the set,
		 * so that lowest distant breadcrumbs are kept in the set.
		 */
		val parentsAlpha = bcA ++ p(bcA) ++ p(p(bcA))
		val nAlpha = parentsAlpha ++ N(parentsAlpha)
		val parentsBeta = bcB ++ p(bcB) ++ p(p(bcB))
		val nBeta = parentsBeta ++ N(parentsBeta)

		val intersection = nAlpha intersect nBeta
		if(intersection.nonEmpty) {
			val gammaToPath = for(gamma <- intersection) yield {
				val pathAlpha : List[g.NodeT] = nAlpha.find(_.equals(gamma)).get.breadcrumbs.reverse
				val pathBeta : List[g.NodeT] = nBeta.find(_.equals(gamma)).get.breadcrumbs
				val completeWalk = Util.joinWalks(g)(pathAlpha,  gamma.node :: pathBeta)
				completeWalk
			}
			val minWalk = gammaToPath.minBy(_.edges.size)
			// Convert to Path, min walk must be a Path
			val pBuild = g.newPathBuilder(minWalk.nodes.head)
			pBuild ++= minWalk.nodes.tail
			Some(pBuild.result())
		} else {
			val possiblePath = ancestorMap.get((alpha.id, beta.id)).map(_.nodes.toSeq).orElse {
				ancestorMap.get((beta.id,alpha.id)).map(_.nodes.toSeq.reverse)
			}
			possiblePath.map { g0Path =>
				val gNodes = g0Path.map { g0Node =>
					nodeMap(g0Node.id)
				}
				// Path is at most of length 3.
				val builder = g.newPathBuilder(gNodes.head)(sizeHint = 3)
				builder.++=(gNodes.tail).result()
			}
		}
	}

	def sphereRouter(g0: Sphere)(ancestorMap: Map[(Int, Int), g0.Path]): Router[Node] = {
		new Router[Node] {
			override def route(g: Graph[Node, UnDiEdge], graphSize: Int)(node1: g.NodeT, node2: g.NodeT, nodeMap: IndexedSeq[g.NodeT]): g.Path = {
				Routing.path(g = g, g0 = g0)(node1, node2, ancestorMap, nodeMap)
			}
		}
	}
}

