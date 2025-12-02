package net.chwthewke.aoc

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Puzzle2Spec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals:
  import Puzzle2.*

  "The interval 38593856-38593862" must {
    "have the sum of invalid ids equal to 38593859" in {
      Interval( 38593856L, 38593862L ).sumInvalidIds must ===( 38593859L )
    }
  }

  "The interval 11-22" must {
    "have the sum of invalid ids equal to 33" in {
      Interval( 11L, 22L ).sumInvalidIds must ===( 33L )
    }
  }

  "The interval 222220-222224" must {
    "have the sum of extended invalid ids equal to 222222" in {
      Interval( 222220L, 222224L ).sumAllInvalidIds must ===( 222222L )
    }
  }

  "The interval 11111110-11111112" must {
    "have the sum of extended invalid ids equal to 11111111" in {
      Interval( 11111110L, 11111112L ).sumAllInvalidIds must ===( 11111111L )
    }
  }

  "The interval 120120-121212" must {
    "have the sum of extended invalid ids equal to 120120 + 121121 + 121212" in {
      Interval( 120120L, 121212L ).sumAllInvalidIds must ===( 120120L + 121121L + 121212L )
    }
  }
