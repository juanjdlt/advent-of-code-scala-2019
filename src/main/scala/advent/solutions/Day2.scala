package advent.solutions

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

/** Day 2: 1202 Program Alarm
  *
  * @see https://adventofcode.com/2019/day/2
  */
object Day2 {

	object Part1 {
		def runIntCode(intCode: List[Int]): List[Int] = {
			@tailrec
			def loop(code: List[Int], optCodePos: Int): List[Int] = {
				
				Try(code(optCodePos)) match {
					case Success(1) => //Sum instrucction
						loop(
							processOptCode(code, optCodePos, _+_),
							optCodePos + 4
						)
					case Success(2) => //Multiply intrucciont
						loop(
							processOptCode(code, optCodePos, _*_),
							optCodePos + 4
						)
					case Success(99) => //Halt Instruction
						code
					case Success(unknownOptCode) =>
						println(s"invalid optcode:${unknownOptCode.toString}")
						List()
					case Failure(_) =>
						//maybe its intCodeEnd, no more codes to process 
						code
				} 	
			}
			loop(intCode, 0)
		}

		private def processOptCode(completeIntCode: List[Int], intCodeIndexPos: Int, operation: (Int, Int) => Int): List[Int] = {
			val codesToProcess = completeIntCode.slice(intCodeIndexPos+1, intCodeIndexPos+4)
			completeIntCode.updated(
				codesToProcess(2), 
				operation(completeIntCode(codesToProcess(0)), completeIntCode(codesToProcess(1)))
			)
		}
	}

	def main(args: Array[String]): Unit = {

		// Copy the puzzle input from https://adventofcode.com/2019/day/2/input
		val gravityAssistIntCodeProgram: List[Int] = List(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,5,19,23,1,23,5,27,1,27,13,31,1,31,5,35,1,9,35,39,2,13,39,43,1,43,10,47,1,47,13,51,2,10,51,55,1,55,5,59,1,59,5,63,1,63,13,67,1,13,67,71,1,71,10,75,1,6,75,79,1,6,79,83,2,10,83,87,1,87,5,91,1,5,91,95,2,95,10,99,1,9,99,103,1,103,13,107,2,10,107,111,2,13,111,115,1,6,115,119,1,119,10,123,2,9,123,127,2,127,9,131,1,131,10,135,1,135,2,139,1,10,139,0,99,2,0,14,0)

		val gravityAssistIntCodeProgram1202AlarmState = (gravityAssistIntCodeProgram.updated(1, 12)).updated(2, 2)
		
		val intCodeResult: List[Int] = Day2.Part1.runIntCode(gravityAssistIntCodeProgram1202AlarmState)
		println("Solution Day2 -part 1-:")
		println(intCodeResult.head)
	}
}