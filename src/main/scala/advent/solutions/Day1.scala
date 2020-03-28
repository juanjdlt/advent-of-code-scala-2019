package advent.solutions

/** Day 1: The Tyranny of the Rocket Equation
  *
  * @see https://adventofcode.com/2019/day/1
  */
object Day1 {

  object Part1 {

    /** Calculates the fuel required to launch a module of a given mass
      *
      * @param mass The mass of the module
      * @return The fuel required to to launch the module
      */
    def fuel(mass: Int): Int = {
      Math.round(mass./(3).toFloat) - 2
    }

    /** Calculates the sum of the fuel required to launch each module of a given mass
      *
      * @param masses The masses of each module
      * @return The sum of the fuel required to launch each module
      */
    def sumOfFuel(
        masses: List[Int]
    ): Int = {
      (calculateFuels _ andThen sumFuels _)(masses)
    }

    //Use the two functions below to complete the exercise
    private def calculateFuels(
        masses: List[Int]
    ): List[Int] = {
      masses.map(fuel(_))
    }

    private def sumFuels(
        fuels: List[Int]
    ): Int = {
      fuels.sum
    }
  }

  object Part2 {

    /** Calculates the total required to launch a module, including the fuel required to launch the fuel itself
      *
      * @param mass The mass of the module
      * @return The fuel required to launch the module, plus the fuel required to launch that fuel
      */
    def totalFuel(mass: Int): Int = {
      val fuelForTheMass = Part1.fuel(mass)

      @scala.annotation.tailrec
      def loop(massOfFuel: Int, massOfFuelAcc: Int): Int = {
        Part1.fuel(massOfFuel) match {
          case neggativeFuel if neggativeFuel <= 0 =>
            massOfFuelAcc
          case possitiveFuel =>
            loop(possitiveFuel, massOfFuelAcc + possitiveFuel)
        }
      }
      val totalFuelForMassesOfFuels = loop(fuelForTheMass, 0)

      fuelForTheMass + totalFuelForMassesOfFuels
    }

    /** Calculates the sum of the total fuel required to launch each module of a given mass
      *
      * @param masses The masses of each module
      * @return The sum of the total fuel required to launch each module
      */
    def sumOfTotalFuel(
        masses: List[Int]
    ): Int = {
      (masses map totalFuel).sum
    }
  }

  // scalastyle:off
  @SuppressWarnings(Array("org.wartremover.warts.All"))
  def main(args: Array[String]): Unit = {

    // Copy the puzzle input from https://adventofcode.com/2019/day/1/input
    val puzzleInput: List[Int] = 102562 :: 138390 :: 145043 :: 86679 :: 120601 :: 58443 :: 54761 :: 81175 :: 127897 :: 69559 :: 56776 :: 145671 :: 69003 :: 119334 :: 130205 :: 77249 :: 74637 :: 92068 :: 66594 :: 90485 :: 140465 :: 73444 :: 107772 :: 107639 :: 144420 :: 58764 :: 56299 :: 66010 :: 84841 :: 83686 :: 139830 :: 136298 :: 135009 :: 136506 :: 61547 :: 73653 :: 136219 :: 138875 :: 95483 :: 91695 :: 146597 :: 121813 :: 131555 :: 145848 :: 139396 :: 141520 :: 54207 :: 86748 :: 98355 :: 67179 :: 59820 :: 137299 :: 92371 :: 74512 :: 110854 :: 111960 :: 63787 :: 114701 :: 63773 :: 127377 :: 128159 :: 120370 :: 138193 :: 106409 :: 135550 :: 107235 :: 56662 :: 99314 :: 69052 :: 131816 :: 138788 :: 96494 :: 73025 :: 148907 :: 85883 :: 86138 :: 86965 :: 55645 :: 119284 :: 80690 :: 69276 :: 116640 :: 108595 :: 50721 :: 94623 :: 93224 :: 137069 :: 130118 :: 97916 :: 82232 :: 137621 :: 97909 :: 74061 :: 140419 :: 101795 :: 69316 :: 64973 :: 90578 :: 118503 :: 100369 :: Nil

    // Solve your puzzle using the functions in parts 1 and 2
    val part1 = Part1.sumOfFuel(puzzleInput)
    println("Solution Part1:")
    println(part1)

    val part2 = Part2.sumOfTotalFuel(puzzleInput)
    println("Solution Part2:")
    println(part2)
  }
  // scalastyle:on
}
