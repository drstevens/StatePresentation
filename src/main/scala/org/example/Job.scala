package org.example

case class Job(s: Int, f: Int, w: Int) {

  // Returns true if other job overlaps with this job
  def overlaps(other: Job): Boolean =
    s <= other.s && other.s < f ||
      s < other.f && other.f <= f ||
      other.s <= s && f <= other.f
}

object JobExamples {

  val sample1 = List(Job(3, 4, 5), Job(1, 3, 1), Job(2, 6, 100), Job(7, 8, 25), Job(4, 5, 50)) //weight = 125
  val sample2 = List(Job(3, 4, 5), Job(1, 3, 1), Job(2, 6, 100), Job(4, 5, 98)) //weight = 104
  val manyJobs = (0 until 16).map(i => Job(i, i + 1, 1)).toList // weight = 16
  val manyJobsWithOverlaps = (0 until 16).map(i => Job(i, i + 2, 1)).toList // weight = 8

}
