import java.nio.ByteBuffer
import scala.language.postfixOps

import Utils._

import scala.annotation.tailrec

/**
  * Simple implementation of GOST R 34.12-2015
  * @param _key key for enc/dec
  * @param _data input data for enc/dec
  */
class Magma private (_key: Array[Byte], _data: Array[Byte]) {
  private val iterKey = expandKey()
  private val data = padding(_data)

  private def this(_key: String, _data: String) {
    this(_key.getBytes, _data.getBytes)
  }

  private def this(_key: Array[Byte], _data: String) {
    this(_key, _data.getBytes)
  }

  private def this(_key: String, _data: Array[Byte]) {
    this(_key.getBytes, _data)
  }

  /**
    * If data not mod 8 bytes (1 block), append '0' byte
    * @param data
    * @return
    */
  @tailrec
  private def padding(data: Array[Byte]): Array[Byte] = {
    if (data.length % 8 != 0) padding(data :+ '\u0000'.toByte)
    else data
  }

  private def expandKey(): Array[Array[Byte]] = {
    @tailrec
    def appendKey(key: Array[Byte]): Array[Byte] = {
      if (key.length < 32) appendKey(key ++ key)
      else key.take(32)
    }

    val key = appendKey(this._key)
    val step = 4
    val iKey = Array.ofDim[Byte](32, 4)

    for (i <- 0 until 24) {
      val range = ((i * step) % 32, (i + 1) * step % 32)
      iKey(i) = ByteBuffer.wrap(key.slice(range _1, if (range._2 == 0) 32 else range _2)).array()
    }

    for ((i, j) <- (24 until 32) zip (0 to 28 by step).reverse) {
      val range = (j, j + step)
      iKey(i) = ByteBuffer.wrap(key.slice(range _1, if (range._2 == 0) 32 else range _2)).array
    }
    if (isDebug) {
      println("iterate keys: ")
      for ((k, i) <- iKey zip (1 to iKey.length)) {
        println(s"K$i = " + byteArrayToHexString(k))
      }
    }
    iKey
  }

  /**
    * Encipher data
    * @return Enciphered data in byte array
    * @see [[Array]]
    */
  private def encipher: Array[Byte] = {
    makeAlg(encipher)
  }

  /**
    * Decipher data
    * @return Deciphered data in byte array
    * @see [[Array]]
    */
  private def decipher: Array[Byte] = {
    makeAlg(decipher).filter(_ != 0)
  }

  /**
    * Enc/Dec 64 bit (8 byte) blocks of input data
    * @param func Function of Dec or Enc
    * @return Dec/Enc array of bytes
    */
  private def makeAlg(func: Array[Byte] => Array[Byte]): Array[Byte] = {
    val blockCount = data.length / 8

    (0 until blockCount).flatMap(i => func(data.slice(i * 8, (i + 1) * 8))).toArray
  }

  /**
    * Single round
    * @param roundKey
    * @param leftBytes left 4 bytes of block
    * @param rightBytes right 4 bytes of block
    * @param isFinRound check the last round
    * @return
    */
  private def round(roundKey: Array[Byte], leftBytes: Array[Byte], rightBytes: Array[Byte], isFinRound: Boolean = false): Array[Byte] = {
    val g = makeG(roundKey, rightBytes) // mod 2 ^ 32
    val t = makeT(g)

    val newRight = new Array[Byte](leftBytes.length)
    val buf = ByteBuffer.allocate(4).putInt((t << 11) | (t >>> 21)).array()

    for (i <- 0 until buf.length) {
      newRight(i) = (buf(i) ^ leftBytes(i)).toByte
    }

    if (!isFinRound) rightBytes ++ newRight else
      newRight ++ rightBytes
  }

  private def makeG(roundKey: Array[Byte], rightBytes: Array[Byte]) = {
    val out = new Array[Byte](4)
    var internal = 0

    for (i <- (0 to 3).reverse) {
      internal = {
        if (roundKey(i) < 0) 256 + roundKey(i) else roundKey(i)
      } + {
        if (rightBytes(i) < 0) 256 + rightBytes(i) else rightBytes(i)
      } + (internal >>> 8)

      out(i) = (internal & 0xff).toByte
    }
    out
  }

  private def makeT(g: Array[Byte]): Int = {
    val res = new Array[Byte](4)

    for (i <- 0 until 4) {
      val firstPart = (g(i) & 0xf0) >>> 4
      val secPart = g(i) & 0x0f
      import Magma.pi
      res(i) = ((pi(i * 2 * 16 + firstPart) << 4) | pi((i * 2 + 1) * 16 + secPart)).toByte
    }

    ByteBuffer.wrap(res).getInt
  }

  private def encipher(block: Array[Byte]): Array[Byte] = {
    makeRound(block)
  }

  private def decipher(block: Array[Byte]): Array[Byte] = {
    makeRound(block, roundNum = iterKey.length - 1, isDecipher = true)
  }

  /**
    *
    * @param data Data's block
    * @param roundNum number of round
    * @param isDecipher flag checks, what'll we do
    * @return
    */
  @tailrec
  private def makeRound(data: Array[Byte], roundNum: Int = 0, isDecipher: Boolean = false): Array[Byte] = {
    val left = data.take(4)
    val right = data.drop(4)

    if (!isDecipher) {
      if (roundNum != iterKey.length - 1) makeRound(round(iterKey(roundNum), left, right), roundNum + 1, isDecipher)
      else round(iterKey(iterKey.length - 1), data.take(4), data.drop(4), isFinRound = true)
    } else {
      if (roundNum != 0) makeRound(round(iterKey(roundNum), left, right), roundNum - 1, isDecipher)
      else round(iterKey(0), data.take(4), data.drop(4), isFinRound = true)
    }

  }

}

object Magma {
  val pi = Array(
    1,7,14,13,0,5,8,3,4,15,10,6,9,12,11,2,
    8,14,2,5,6,9,1,12,15,4,11,0,13,10,3,7,
    5,13,15,6,9,2,12,10,11,7,8,1,4,3,14,0,
    7,15,5,10,8,1,6,13,0,9,3,14,11,4,2,12,
    12,8,2,1,13,4,15,6,7,0,10,5,3,14,9,11,
    11,3,5,8,2,15,10,13,14,1,7,4,12,9,6,0,
    6,8,2,3,9,10,5,12,1,14,4,7,11,13,0,15,
    12,4,6,2,10,5,11,9,14,8,13,7,0,3,15,1
  )

//  private def apply(key: Array[Byte], data: Array[Byte]): Magma = new Magma(key, data)
//  private def apply(key: String, data: String): Magma = new Magma(key, data)
//  private def apply(key: Array[Byte], data: String): Magma = new Magma(key, data)
//  private def apply(key: String, data: Array[Byte]): Magma = new Magma(key, data)

  def encipher(key: String, data: Array[Byte]): Array[Byte] = new Magma(key, data).encipher
  def encipher(key: Array[Byte], data: String): Array[Byte] = new Magma(key, data).encipher
  def encipher(key: String, data: String): Array[Byte] = new Magma(key, data).encipher
  def encipher(key: Array[Byte], data: Array[Byte]): Array[Byte] = new Magma(key, data).encipher

  def decipher(key: String, data: Array[Byte]): Array[Byte] = new Magma(key, data).decipher
  def decipher(key: Array[Byte], data: String): Array[Byte] = new Magma(key, data).decipher
  def decipher(key: String, data: String): Array[Byte] = new Magma(key, data).decipher
  def decipher(key: Array[Byte], data: Array[Byte]): Array[Byte] = new Magma(key, data).decipher
}
