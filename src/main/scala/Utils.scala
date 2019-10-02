import java.nio.ByteBuffer

import scala.collection.mutable.ArrayBuffer

object Utils {
  // Debug flag
  var isDebug = false

  /**
    * Convert array of bytes to hex string
    * @param bytes input array
    * @return hex string split with space
    * @example 0x12345678 0x78945612 0x45612378
    */
  def byteArrayToHexString(bytes: Array[Byte]): String = {
    val tmpBytes = ArrayBuffer[Byte]()
    val blockSize = 4
    val blocks = {
      if (bytes.length % blockSize != 0) {
        tmpBytes.append((0 until blockSize - bytes.length % blockSize).map(_ => 0.toByte).toArray :_ *)
        bytes.length / blockSize + 1
      } else bytes.length / blockSize
    }
    tmpBytes.append(bytes :_ *)
    val tmpArr = new Array[Byte](blockSize)


    val resArr = ArrayBuffer[Int]()

    for (i <- 0 until blocks) {
      for (j <- tmpArr.indices) {
        tmpArr(j) = tmpBytes(i * blockSize + j)
      }
      resArr += ByteBuffer.wrap(tmpArr).getInt
    }

    resArr.map(_.toHexString).mkString("0x", " 0x", "")

  }


  def mapFromIntArrayToByteArray(arr: Array[Int]): Array[Byte] = {
    arr.flatMap(i => ByteBuffer.allocate(4).putInt(i).array)
  }
}
