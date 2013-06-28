package org.example

import math._

object WeightedIntervalScheduling {

  def naiveOptimumSchedule(jobs: List[Job]): Int = {
    def recurse: List[Job] => Int = {
      case Nil => 0
      case i :: rest =>
        max(recurse(rest.filterNot(i.overlaps)) + i.w,
                 recurse(rest))
    }
    recurse(jobs.sortBy(_.s))
  }

  def memoOptimumSchedule(jobs: List[Job]): Int = {
    type Memo = Map[Job, Int]
    def recurse: (Memo, List[Job]) => (Memo, Int) = {
      case (memo, Nil) => (memo, 0)
      case (memo, i :: rest) if memo.contains(i) => (memo, memo(i))
      case (memo, i :: rest) =>
        val (m1, w1) = recurse(memo, rest.filterNot(i.overlaps))
        val (m2, w2) = recurse(m1, rest)
        val w = max(w1 + i.w, w2)
        (m2 + (i -> w), w)
    }

    recurse(Map(), jobs.sortBy(_.s))._2
  }

  def stateOptimumSchedule(jobs: List[Job]): Int = {
    type Memo = Map[Job, Int]
    def recurse: List[Job] => State[Memo, Int] = {
        case Nil => State.state(0)
        case i :: rest =>
          State {
            memo =>
              val (m1, w1) = recurse(rest.filterNot(i.overlaps)).run(memo)
              val (m2, w2) = recurse(rest).run(m1)
              val w = max(w1 + i.w, w2)
              (m2 + (i -> w), w)
          }
      }
    recurse(jobs.sortBy(_.s)).eval(Map())
  }

  def state2OptimumSchedule(jobs: List[Job]): Int = {
    type Memo = Map[Job, Int]
    def recurse: List[Job] => State[Memo, Int] = {
          case Nil => State.state(0)
          case i :: rest =>
            for {
              memoized <- State.gets((_: Memo) get i)
              optimum <- memoized map State.state[Memo, Int] getOrElse(for {
                w1 <- recurse(rest.filterNot(i.overlaps))
                w2 <- recurse(rest)
                w = max(w1 + i.w, w2)
                _ <- State.modify((_: Memo) + (i -> w))
              } yield w)

            } yield optimum
        }

    recurse(jobs.sortBy(_.s)).eval(Map.empty)
  }


  def naiveStateCountOptimumSchedule(jobs: List[Job]): (Int, Int) = {
    def recurse: List[Job] => State[Int, Int] = {
      case Nil => State.state(0)
      case i :: rest =>
        for {
          _ <- State.modify[Int](_ + 1)
          w1 <- recurse(rest.filterNot(i.overlaps))
          w2 <- recurse(rest)
        } yield max(w1 + i.w, w2)
    }
    recurse(jobs.sortBy(_.s)).run(0)
  }

  def stateCountOptimumSchedule(jobs: List[Job]): (Int, Int) = {
    import scalaz._
    import scalaz.std.map._
    import scalaz.std.tuple._
    import scalaz.std.anyVal._
    import scalaz.syntax.state._

    type MemoMap = Map[Job, Int]
    type Memo = (Int, MemoMap)
    def recurse: List[Job] => State[Memo, Int] = {
        case Nil => State.state(0)
        case i :: rest =>
          for {
            memoized <- Lens.secondLens[Int, MemoMap] map(_ get i)
            optimum <- memoized map State.state[Memo, Int] getOrElse(for {
              _ <- Lens.firstLens.mods((_: Int) + 1)
              w1 <- recurse(rest.filterNot(i.overlaps))
              w2 <- recurse(rest)
              o = max(w1 + i.w, w2)
              _ <- Lens.secondLens.mods((_: MemoMap) + (i -> o))
            } yield o)
          } yield optimum
      }

    val ((count, _), weight) = recurse(jobs.sortBy(_.s)).runZero[Memo]
    count -> weight
  }
}
