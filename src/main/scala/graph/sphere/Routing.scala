package graph.sphere

import graph.Util
import instrumentation.Metric.Router

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.immutable.Graph
import Units._

object Routing extends Router[Node] {
	override def route(g: Sphere, graphSize: Int)(node1: g.NodeT, node2: g.NodeT, nodeMap: IndexedSeq[g.NodeT]): g.Path = {
		pathRecursive(g)(node1, node2, List.empty, List.empty)
	}

	@tailrec
	def pathRecursive(g: Sphere)(alpha: g.NodeT, beta: g.NodeT, pathA: List[g.NodeT], pathB: List[g.NodeT]) : g.Path = {
		val localPath = localSearch(g)(alpha, beta)
		localPath match {
			case Some(pab) =>
				val joinedPaths = Util.joinPaths(g)((alpha :: pathA).reverse, pab.nodes, beta :: pathB)
				joinedPaths
			case None =>
				if(beta.layer > alpha.layer) {
					pathRecursive(g)(alpha, incrementPath(g)(beta), pathA, beta :: pathB)
				} else {
					pathRecursive(g)(incrementPath(g)(alpha), beta, alpha :: pathA, pathB)
				}
		}
	}

	def incrementPath(g: Sphere)(v: g.NodeT) : g.NodeT = {
		def pGood(vertices: Set[g.NodeT]): Set[g.NodeT] = {
			val parents = vertices.flatMap{ alpha =>
				SphereNode.parents(g)(alpha).toSet
			}

			// Each parent must be on the lowest layer of all parents.
			parents.filter( _.layer <= parents.map(_.layer).max )
		}

		val bSet = pGood(Set(v))

		// calculate f(aSet)
		val potentialParents = if(bSet.head.layer == 0) {
			bSet
		} else {
			val bSetGood = pGood(bSet)
			bSet.filter { beta =>
				val betaP = SphereNode.parents(g)(beta).toSet
				// Check whether p(beta) intersect pGood(bSet) is nonEmpty
				(betaP intersect bSetGood).nonEmpty
			}
		}

		// Pick a random element
		Random.shuffle(potentialParents.toSeq).head
	}

	def localSearch(g: Sphere)(alpha : g.NodeT, beta: g.NodeT) : Option[g.Path] = {
		val nAlpha = alpha.withMaxDepth(6)
		nAlpha.find(_ == beta).flatMap(_ => alpha.shortestPathTo(beta))
		/*
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
		val dist1Alpha = bcA ++ N(bcA)
		val dist2Alpha = dist1Alpha ++ N(dist1Alpha)
		val dist3Alpha = dist2Alpha ++ N(p(p(bcA)))
//		val parentsAlpha = bcA ++ p(bcA) ++ p(p(bcA))
//		val nAlpha = parentsAlpha ++ N(parentsAlpha)
//		val nnAlpha = nAlpha ++ N(nAlpha)
		val dist1Beta = bcB ++ N(bcB)
		val dist2Beta = dist1Beta ++ N(p(bcB))
		val dist3Beta = dist2Beta ++ N(p(p(bcB)))
//		val parentsBeta = bcB ++ p(bcB) ++ p(p(bcB))
//		val nBeta = parentsBeta ++ N(parentsBeta)

		val intersection = dist3Alpha intersect dist3Beta
		if(intersection.nonEmpty) {
			val gammaToPath = for(gamma <- intersection) yield {
				val pathAlpha : List[g.NodeT] = dist3Alpha.find(_.equals(gamma)).get.breadcrumbs.reverse
				val pathBeta : List[g.NodeT] = dist3Beta.find(_.equals(gamma)).get.breadcrumbs
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
		*/
	}

}

