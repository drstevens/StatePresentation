package org.example

import math._

object Demo {

  def optimumSchedule1(jobs: List[Job]): Int = {
    def recurse: List[Job] => Int = {
      case Nil => 0
      case i :: rest =>
        val m = max(recurse(rest.filterNot(i.overlaps)) + i.w,
          recurse(rest))
        println("i: %12s, max: %s".format(i, m))
        m
    }
    recurse(jobs.sortBy(_.s))
  }

  def optimumSchedule2(jobs: List[Job]): Int = {
    type Memo = Map[Job, Int]
    def recurse: (Memo, List[Job]) => (Memo, Int) = {
      case (memo, Nil) => memo -> 0
      case (memo, i :: rest) if memo.contains(i) => memo -> memo.apply(i)
      case (memo, i :: rest) =>
        val (m1, w1) = recurse(memo, rest.filterNot(i.overlaps))
        val (m2, w2) = recurse(m1, rest)
        val w = max(w1 + i.w, w2)
        println("i: %12s, max: %s".format(i, w))
        (m2 + (i -> w), w)
    }
    recurse(Map.empty, jobs.sortBy(_.s))._2
  }

  def optimumSchedule3(jobs: List[Job]): Int = {
    type Memo = Map[Job, Int]
    def recurse: List[Job] => State[Memo, Int] = {
      case Nil => State.state(0)
      case i :: rest => State { memo =>
        memo.get(i).map(w => (memo, w)).getOrElse {
          val (m1, w1) = recurse(rest.filterNot(i.overlaps)).run(memo)
          val (m2, w2) = recurse(rest).run(m1)
          val w = max(w1 + i.w, w2)
          println("i: %12s, max: %s".format(i, w))
          (m2 + ((i, w)), w)
        }
      }
    }
    recurse(jobs.sortBy(_.s)).eval(Map.empty)
  }

  def optimumSchedule4(jobs: List[Job]): Int = {
    type Memo = Map[Job, Int]
    def recurse: List[Job] => State[Memo, Int] = {
      case Nil => State.state(0)
      case i :: rest => State { memo =>
        memo.get(i).map(w => (memo, w)).getOrElse {
          for {
            w1 <- recurse(rest.filterNot(i.overlaps))
            w2 <- recurse(rest)
            w = max(w1 + i.w, w2)
            _ <- State.modify(m => m + (i -> w))
          } yield w
        }
      }
      }
    recurse(jobs.sortBy(_.s)).eval(Map.empty)
  }


  def optimumSchedule5(jobs: List[Job]): Int = {
    0
  }
  def optimumSchedule6(jobs: List[Job]): Int = {
    0
  }
  def optimumSchedule7(jobs: List[Job]): Int = {
    0
  }
  def optimumSchedule8(jobs: List[Job]): Int = {
    0
  }
  def optimumSchedule9(jobs: List[Job]): Int = {
    0
  }
}
