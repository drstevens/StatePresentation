package org.example

case class State[S, A](run: S => (S, A)) {

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (s1, a) = run(s) // evaluate current state
      (s1, f(a))           // apply f to value
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (s1, a) = run(s) // evaluate current state
      val s2 = f(a)        // apply f to new value resulting in new state
      s2 run s1            // evaluate new state using previously evaluated state
    })

  // Evaluate State, drop resulting state
  def eval(s: S): A = run(s)._2
}

object State {

  // Wrap `a` in State
  def state[S, A](a: A): State[S, A] =
    State(s => (s, a))

  // Retrieve State as value by applying function to existing state
  def gets[S, A](f: S => A): State[S, A] =
    State(s => (s, f(s)))

  // Modify the current State, returning unit as State value
  def modify[S](f: S => S): State[S, Unit] =
    State(s => (f(s), ()))
}
