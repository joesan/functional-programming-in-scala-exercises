/**
 * This is not part of the book, but I wanted to explore and document
 * type level programming in Scala. I will try to explain this with
 * the use of Peano numbers [[ https://wiki.haskell.org/Peano_numbers ]]
 */
 
 // Let's first draw an implementation of Peano numbers at the value level:
 sealed trait PeanoNum { def + (that: Nat): Nat }
 case object Zero extends PeanoNum {
   def + (that: Nat): Nat = that
 }
 case object Next(n: Nat) extends PeanoNum {
   def + (that: Nat): Nat = Nat(n + that) 
 }
