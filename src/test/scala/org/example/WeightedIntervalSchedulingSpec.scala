package org.example

import org.specs2.mutable.Specification

class WeightedIntervalSchedulingSpec extends Specification {

  import WeightedIntervalScheduling._
  import JobExamples._

  "Job.overlaps" should {
    "correctly evaluate" in {
      Job(1, 2, 0).overlaps(Job(2, 3, 0)) ==== false
      Job(2, 4, 0).overlaps(Job(1, 2, 0)) ==== false
      Job(1, 4, 0).overlaps(Job(2, 3, 0)) ==== true
      Job(2, 4, 0).overlaps(Job(1, 3, 0)) ==== true
      Job(2, 4, 0).overlaps(Job(1, 5, 0)) ==== true
      Job(2, 6, 0).overlaps(Job(3, 4, 0)) ==== true
    }
  }

  val tests = List[(List[Job], Int)](
    (Nil, 0),
    (List(Job(3, 4, 5), Job(1, 3, 1), Job(2, 6, 100), Job(4, 5, 50)), 100),
    (List(Job(3, 4, 5), Job(1, 3, 1), Job(2, 6, 100), Job(4, 5, 98)), 104),
    (manyJobs, manyJobs.length),
    (manyJobsWithOverlaps, manyJobsWithOverlaps.length / 2))

  "naiveOptimumSchedule" should {
    "return optimum schedule" in {
      tests.foreach {
        case (input, expected) => naiveOptimumSchedule(input) ==== expected
      }
    }
  }

  "memoOptimumSchedule" should {
    "return optimum schedule" in {
      tests.foreach {
        case (input, expected) => memoOptimumSchedule(input) ==== expected
      }
    }
  }

  "stateOptimumSchedule" should {
    "return optimum schedule" in {
      tests.foreach {
        case (input, expected) => stateOptimumSchedule(input) ==== expected
      }
    }
  }

  "state2OptimumSchedule" should {
    "return optimum schedule" in {
      tests.foreach {
        case (input, expected) => state2OptimumSchedule(input) ==== expected
      }
    }
  }

  "stateCountOptimumSchedule" should {
    "return optimum schedule along with number of recursions" in {
      stateCountOptimumSchedule(manyJobs) ==== manyJobs.length -> manyJobs.length
    }
  }

  "naiveStateCountOptimumSchedule" should {
    "return optimum schedule along with number of recursions" in {
      naiveStateCountOptimumSchedule(manyJobs) ==== ((math.pow(2, manyJobs.length).asInstanceOf[Int] - 1, manyJobs.length))
    }
  }

}
