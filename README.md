The prototype of a binary typelevel unsigned integer implementation for Scala.

```scala
import uint._
import java.lang.Long.parseLong

val a = parseLong("101001100",2)
val b = parseLong(  "1100001",2)

type A = I°O°I°O°O°I°I°O°O
type B =     I°I°O°O°O°O°I

println(a*b)
println( toLong[A*B] )
```
