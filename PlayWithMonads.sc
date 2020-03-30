/**
 * ===========================================================================
 * Monads consider it as a container or wrapper types have certain properties:
 * ===========================================================================
 *
 *  1. A parametrized type — e.g, Option[T]
 *
 *  2. Unit (return) — e.g, Option.apply         Unit is defined as: A => M[A]
 *
 *  3. FlatMap (bind) —e.g, Option.flatMap       FlatMap is defined as: (M[A], f: A => M[B]) => M[B]
 *
 * */

/**
    List Monad in scala
 */

def makeListOfDoubles(int: Int): List[Double] =  List(int.toDouble)

val l1 = List.apply(1, 2, 3)  // unity rule

val l2 = List("1st")

l1.flatMap(makeListOfDoubles)  //  f: (A => M[B]) => M[B]

l1.map(makeListOfDoubles) // f(A => M[B]) => M[M[A]]

/**
     Option Monad in scala
 */

val a =  Option(1)  // unity rule

val x = Option.apply("someThing") // parametrized Option[+A]

x.flatMap(a => Option(1)) // def flatMap[B](f: A => Option[B]): Option[B]

x.map(a =>  Option(1)) // def map[B](f: A => B): Option[B]


def combine(a: Int, b: Int, c: Int): Int = {a + b + c}

val m1 = List(1, 2, 3)
val m2 = List(1,2)
val m3 = List(1)

m1.flatMap(a =>
  m2.flatMap(b =>
    m3.map(c => combine(a,b,c))
  )
)

// this is equivalent to this expression using for-comprehension

for {
  a <- m1
  b <- m2
  c <- m3
} yield combine(a,b,c)







