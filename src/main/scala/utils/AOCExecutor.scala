package utils

import utils.Profiler.{printTime, time}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
  abstract class AOCExecutor {
    def part1(): Unit

    def part2(): Unit

    def execute: Unit ={
      println("---------- part 1 -----------")
      val duration1 = time (part1)
      println(s"duration part 1: ${duration1.toMicros} µs")
      println("---------- part 2 -----------")
      val duration2 = time (part2)
      println(s"duration part 2: ${duration2.toMicros} µs")

    }
  }

