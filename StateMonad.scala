package monads.withmonads

import monads.PriceState

object StateMonad {

  object State {

    def state[S, A](f: S => (S, A)):State[S,A] = // transit from State A to State B with transition function  M[A], f(A) => M[B]
      new State[S, A] { def apply(s: S): (S,A) = f(s) }

    def init[S]: State[S, S] = state[S, S](s => (s, s))

    def modify[S](f: S => S):State[S, Unit] =  init[S] flatMap (s => state( _ => ( f(s), () )))

    def gets[S,A](f: S => A): State[S, A]   =  init[S] flatMap (s => state( _ => ( s, f(s) )))

  }

  trait State[S, +A] {  // define a state monad of parameterized type S

    import State._

    def apply(s: S): (S, A) // unity rule

    def map[B](f: A => B): State[S, B] =
      state( // wrap as a monad
        apply(_) match {
        case (s, a) => (s, f(a))
      })

    def flatMap[B](f: A => State[S, B]): State[S, B] =  // 3. flatMap Rule  M[A], f(A) => M[B]
      state(
        apply(_) match {
        case (s, a) => f(a)(s)
      })

  }

  def main(args: Array[String]): Unit = {
    import State._

    val f = (x:PriceState) => x.price + 10

    val x: State[PriceState, Unit] = modify[PriceState](s => s.copy(price = f(s)))
    val y: State[PriceState, PriceState] = state[PriceState, PriceState](s => (s, s.copy(price = f(s))))
    val z: State[PriceState, Double] = y.map(x => 1)
    val m: State[PriceState, Int] = y.flatMap(x => state( _ => (x , 10)))


    println(x.apply(PriceState("i", "d", 0.0))._2)
    println(y.apply(PriceState("i", "d", 0.0))._2)
    println(z.apply(PriceState("i", "d", 0.0))._2)
    println(m.apply(PriceState("i", "d", 0.0))._2)

  }
}