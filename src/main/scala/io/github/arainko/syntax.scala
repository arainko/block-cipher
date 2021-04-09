package io.github.arainko

object syntax {

  implicit class IntOps(private val int: Int) extends AnyVal {
    def %% (ring: Int): Int = Math.floorMod(int, ring)
  }
}
