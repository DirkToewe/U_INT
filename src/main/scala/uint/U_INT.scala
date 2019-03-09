package uint

import scala.language.higherKinds

sealed class U_INT
{
  type Match[
    ReturnType,
    OnConcat[_ <: U_INT,_ <: BIT] <: ReturnType,
    OnBit[_ <: BIT] <: ReturnType
  ] <: ReturnType
  type Plus1 <: U_INT
  type Plus[UInt <: U_INT] <: U_INT
  type Times[UInt <: U_INT] <: U_INT
}
sealed class BIT extends U_INT
{
  override type Match[
    ReturnType,
    OnConcat[_ <: U_INT,_ <: BIT] <: ReturnType,
    OnBit[_ <: BIT] <: ReturnType
  ] = OnBit[this.type]

  type MatchBit[
    ReturnType,
    OnZero <: ReturnType,
    OnOne <: ReturnType
  ] <: ReturnType
}
final class O extends BIT
{
  override type MatchBit[
    ReturnType,
    OnZero <: ReturnType,
    OnOne <: ReturnType
  ] = OnZero
  override type Plus1 = I
  override type Plus[UInt <: U_INT] = UInt
  override type Times[UInt <: U_INT] = O
}
final class I extends BIT
{
  override type MatchBit[
    ReturnType,
    OnZero <: ReturnType,
    OnOne <: ReturnType
  ] = OnOne
  override type Plus1 = I°O
  override type Plus[UInt <: U_INT] = UInt#Plus1
  override type Times[UInt <: U_INT] = UInt
}
final class ° [Lhs <: U_INT, Bit <: BIT] extends U_INT
{
  override type Match[ ReturnType, OnConcat[_ <: U_INT,_ <: BIT] <: ReturnType, OnBit[_ <: BIT] <: ReturnType ]
    = OnConcat[Lhs,Bit]

  override type Plus1 = Bit#MatchBit[
    _°_,
    Lhs      °I,
    Lhs#Plus1°O
  ]

  type          Plus_BIT[ Bit <: BIT ] = Bit#MatchBit[ _°_, this.type, Plus1 ]
  type          Plus_°[L <: U_INT, B <: BIT] = ((L#Plus[Lhs])°B)#Plus_BIT[Bit]
  override type Plus[UInt <: U_INT] = UInt#Match[
    _°_,
    Plus_°,
    Plus_BIT
  ]
  type Times[UInt <: U_INT] = Bit#MatchBit[
    U_INT,
    Lhs#Times[UInt°O],
    Lhs#Times[UInt°O]#Plus[UInt]
  ]
}
