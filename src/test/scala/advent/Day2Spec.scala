package advent

import org.scalatest._
import advent.solutions._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class Day2Spec extends AnyFlatSpec with Matchers {

	it should "return:(2,0,0,0,99) for an IntCode:(1,0,0,0,99)" in {
		val testIntCode: List[Int] = List(1,0,0,0,99)
		Day2.Part1.runIntCode(testIntCode) should be(List(2,0,0,0,99))
	}

	it should "return:(2,3,0,6,99) for an IntCode:(2,3,0,3,99)" in {
		val testIntCode: List[Int] = List(2,3,0,3,99)
		Day2.Part1.runIntCode(testIntCode) should be(List(2,3,0,6,99))
	}

	it should "return:(2,4,4,5,99,9801) for an IntCode:(2,4,4,5,99,0)" in {
		val testIntCode: List[Int] = List(2,4,4,5,99,0)
		Day2.Part1.runIntCode(testIntCode) should be(List(2,4,4,5,99,9801))
	}

	it should "return:(30,1,1,4,2,5,6,0,99) for an IntCode:(1,1,1,4,99,5,6,0,99)" in {
		val testIntCode: List[Int] = List(1,1,1,4,99,5,6,0,99)
		Day2.Part1.runIntCode(testIntCode) should be(List(30,1,1,4,2,5,6,0,99))
	}

}