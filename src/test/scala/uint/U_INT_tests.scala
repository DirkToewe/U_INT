package uint

import shapeless.Lazy
import utest._

object U_INT_tests extends TestSuite
{
  private trait Next1[UInt <: U_INT]
  {
    @inline def apply( i: Long ): Unit
  }

  private trait Next2[II <: U_INT, JJ <: U_INT]
  {
    @inline def apply( i: Long, j: Long ): Unit
  }

  override def tests: Tests = Tests{

    'toLong {
      assert(  0 == toLong[        O] )
      assert(  1 == toLong[        I] )
      assert(  2 == toLong[      I°O] )
      assert(  3 == toLong[      I°I] )
      assert(  4 == toLong[    I°O°O] )
      assert(  5 == toLong[    I°O°I] )
      assert(  6 == toLong[    I°I°O] )
      assert(  7 == toLong[    I°I°I] )
      assert(  8 == toLong[  I°O°O°O] )
      assert(  9 == toLong[  I°O°O°I] )
      assert( 10 == toLong[  I°O°I°O] )
      assert( 11 == toLong[  I°O°I°I] )
      assert( 12 == toLong[  I°I°O°O] )
      assert( 13 == toLong[  I°I°O°I] )
      assert( 14 == toLong[  I°I°I°O] )
      assert( 15 == toLong[  I°I°I°I] )
      assert( 16 == toLong[I°O°O°O°O] )
      assert( 17 == toLong[I°O°O°O°I] )
      assert( 18 == toLong[I°O°O°I°O] )
      assert( 19 == toLong[I°O°O°I°I] )
      assert( 20 == toLong[I°O°I°O°O] )
      assert( 21 == toLong[I°O°I°O°I] )
      assert( 22 == toLong[I°O°I°I°O] )
      assert( 23 == toLong[I°O°I°I°I] )
      assert( 24 == toLong[I°I°O°O°O] )
      assert( 25 == toLong[I°I°O°O°I] )
      assert( 26 == toLong[I°I°O°I°O] )
      assert( 27 == toLong[I°I°O°I°I] )
      assert( 28 == toLong[I°I°I°O°O] )
      assert( 29 == toLong[I°I°I°O°I] )
      assert( 30 == toLong[I°I°I°I°O] )
      assert( 31 == toLong[I°I°I°I°I] )
    };

    'Plus1 {
      implicit lazy val end: Next1[I°O°O°O ° O°O°O°O ° O°O°O°O] = i => ()
      @inline implicit def next[UInt <: U_INT]( implicit mat: Materialized_U_INT[UInt], next: Lazy[Next1[UInt#Plus1]] ): Next1[UInt]
        = i => loop[UInt](i)(mat, next.value)

      def loop[UInt <: U_INT]( i: Long )( implicit mat: Materialized_U_INT[UInt], next: Next1[UInt#Plus1] ): Unit =
      {
//        println(i)
        assert( i == toLong[UInt] )
        next(i+1)
      }

      loop[O](0)
    };

    Symbol("Plus#1") {
      type A = I ° I°O°I°I ° O°I°O°O ° O°I°I°O ° I°O°I°I ° O°O°O°I
      type B = I ° O°O°O°O ° I°I°O°I ° O°O°O°O ° I°I°O°O ° O°I°I°O ° O°I°O°O
      val a = 1337*1337L
      val b = 42*1337*314L
      assert{ a+b   == toLong[A+B  ]}
      assert{ a+b+a == toLong[A+B+A]}
      assert{   b+a == toLong[  B+A]}
    };

    Symbol("Plus#2") {
      type END = I°O ° O°O°O°O
      implicit lazy val end: Next2[END,END] = (i,j) => ()

      @inline implicit def nextOuter[II <: U_INT]( implicit ij: Materialized_U_INT[II#Plus1 + O], next: Lazy[Next2[II#Plus1,O]] ): Next2[II,END]
        = (i,j) => loop[II#Plus1,O](i+1,0)( ij, next.value )

      @inline implicit def nextInner[II <: U_INT, JJ <: U_INT]( implicit ij: Materialized_U_INT[II + JJ#Plus1], next: Lazy[Next2[II,JJ#Plus1]] ): Next2[II,JJ]
        = (i,j) => loop[II,JJ#Plus1](i,j+1)( ij, next.value )

      def loop[II <: U_INT, JJ <: U_INT]( i: Long, j: Long )( implicit ij: Materialized_U_INT[II + JJ], next: Next2[II,JJ] ): Unit =
      {
//        println(i,j)
        assert( i+j == toLong[II + JJ] )
        next(i,j)
      }

      loop[O,O](0,0)
    };

    Symbol("Times#1") {
      type A = I°I°O°I°I°O°I°O°O°O°I°I°O°I°O°I°I°O°O°O°I
      type B = I°O°O°O°O°I°I°O°I°O°O°O°O°I°I°O°O°O°I°I°O°O°I°O°O
      val a = 1337*1337L
      val b = 42*1337*314L
      assert{ a*b == toLong[A*B] }
      assert{ b*a == toLong[B*A] }
    };

    Symbol("Times#2") {
      type END = I°O ° O°O°O°O
      implicit lazy val end: Next2[END,END] = (i,j) => ()

      implicit def nextOuter[II <: U_INT]( implicit ij: Materialized_U_INT[II#Plus1 * O], next: Lazy[Next2[II#Plus1,O]] ): Next2[II,END]
        = (i,j) => loop[II#Plus1,O](i+1,0)( ij, next.value )

      implicit def nextInner[II <: U_INT, JJ <: U_INT]( implicit ij: Materialized_U_INT[II * JJ#Plus1], next: Lazy[Next2[II,JJ#Plus1]] ): Next2[II,JJ]
        = (i,j) => loop[II,JJ#Plus1](i,j+1)( ij, next.value )

      def loop[II <: U_INT, JJ <: U_INT]( i: Long, j: Long )( implicit ij: Materialized_U_INT[II * JJ], next: Next2[II,JJ] ): Unit =
      {
//        println(i,j)
        assert( i*j == toLong[II * JJ] )
        next(i,j)
      }

      loop[O,O](0,0)
    };

  }
}
