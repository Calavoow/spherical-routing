package graph

import org.scalatest.{Matchers, FlatSpec}

class UnitsSpec extends FlatSpec with Matchers {
	import graph.Util.RichGraph
	"The Icosahedron" should "have a diameter of 3" in {
		Units.icosahedron.diameter should be (3)
	}
}
