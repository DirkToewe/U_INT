package object uint
{
  @inline type + [A <: U_INT, B <: U_INT] = A#Plus[B]
  @inline type * [A <: U_INT, B <: U_INT] = A#Times[B]

  case class Materialized_U_INT[UInt <: U_INT]( value: Long ){}

  implicit val materialized_ZERO = Materialized_U_INT[O](0)
  implicit val materialized_ONE  = Materialized_U_INT[I](1)
  implicit def materialized_° [Lhs <: U_INT, Bit <: BIT]
                              ( implicit lhs: Materialized_U_INT[Lhs],
                                         bit: Materialized_U_INT[Bit] ): Materialized_U_INT[Lhs ° Bit]
    = Materialized_U_INT apply[Lhs ° Bit] lhs.value*2 + bit.value

  def toLong[UInt <: U_INT]( implicit uInt: Materialized_U_INT[UInt] ): Long
    = uInt.value
}
