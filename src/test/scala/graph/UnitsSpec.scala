package graph

import org.scalatest.{FlatSpec, Matchers}

class UnitsSpec extends FlatSpec with Matchers {
	"The Icosahedron" should "have a diameter of 3" in {
		Units.icosahedron.diameter should be (3)
	}
}
