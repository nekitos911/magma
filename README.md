# About
Scala library for use "Magma" encryption

## Build

Use for build solution in jar:
```sh
sbt package
```
Use for run tests:
```sh
// run all tests
sbt test

//run single test
sbt test:testOnly *TestClass
```

## Usage example
Just call instance of Magma class with needed method: <br>
```scala
// import dependency
import Magma._

//...

// init key and data
// it can be String or Array[Byte]
val key = "exampleKey"
val data = "exampleData"

// enciphered result
// res is Array[Byte]
val enc = Magma.encipher(key, data)

//...

// deciphered result
// also Array[Byte]
val dec = Magma.decipher(key, enc)
```

Also library allows map byte array to hex string:<br>
```scala

//...

val arr = Array[Byte](1, 2, 3, 4)
val hex = Utils.byteArrayToHexString(arr)
// hex = 0x102030
```
Also map from int array to byte array
```scala
val arr = Utils.mapFromIntArrayToByteArray(Array (0xfedcba98))
// arr = {-2, -36, -70, -104}

```