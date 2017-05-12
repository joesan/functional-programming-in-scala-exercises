/**
 * This is not part of the book, but I wanted to explore and document
 * type level programming in Scala. I will try to explain this with
 * the use of Peano numbers [[ https://wiki.haskell.org/Peano_numbers ]]
 */
 
 // Let's first draw an implementation of Peano numbers at the value level:
 sealed trait PeanoNum { def + (that: PeanoNum): PeanoNum }

 case object Zero extends PeanoNum {
   def + (that: PeanoNum): PeanoNum = that
 }

 case class Next(n: PeanoNum) extends PeanoNum {
   def + (that: PeanoNum): PeanoNum = Next(n + that) 
 }

 // Now some test cases to see how our value types hold up!
scala> val zero = Zero
zero: Zero.type = Zero

scala> val one = Next(Zero)
one: Next = Next(Zero)

scala> val two = Next(one)
two: Next = Next(Next(Zero))

scala> val three = Next(two)
three: Next = Next(Next(Next(Zero)))

scala> val four = Next(three)
four: Next = Next(Next(Next(Next(Zero))))

scala> one + two == three
res0: Boolean = true

scala> two + three == four
res1: Boolean = false

scala> three + one == four
res2: Boolean = true

// so far so good!! Now let's translate this into types instead of values!!
