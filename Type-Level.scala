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

sealed trait PeanoNumType { // Type at the end indicates to the reader that we are dealing with types
  type plus[That <: PeanoNumType] <: PeanoNumType
}

sealed trait ZeroType extends PeanoNumType {
  type plus[That <: PeanoNumType] = That
}

sealed trait NextType[This <: PeanoNumType] extends PeanoNumType {
   type plus[That <: PeanoNumType] = NextType[This#plus[That]]
}

// Let's now write some test cases to see if our types hold up! If we get past the compiler, our test cases pass
scala> type One = NextType[ZeroType]
defined type alias One

scala> type Two = NextType[One]
defined type alias Two

scala> type Three = NextType[Two]
defined type alias Three

scala> type Four = NextType[Three]
defined type alias Four

scala> implicitly[ZeroType =:= ZeroType]
res0: =:=[ZeroType,ZeroType] = <function1>

// I'm making it to explicitly fail so that you know how to use the type systax
scala> implicitly[ZeroType plus One =:= One] 
<console>:16: error: not found: type plus
       implicitly[ZeroType plus One =:= One]
                           ^

scala> implicitly[ZeroType#plus[One] =:= One]
res2: =:=[One,One] = <function1>

scala> implicitly[One#plus[Two] =:= Three]
res3: =:=[NextType[Two],Three] = <function1>

// see, we cannot get past the compiler, so our test case fails!
scala> implicitly[Two#plus[Three] =:= Four]
<console>:19: error: Cannot prove that NextType[NextType[Three]] =:= Four.
       implicitly[Two#plus[Three] =:= Four]
