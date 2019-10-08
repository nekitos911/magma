import MagmaTest._
import Utils.{byteArrayToHexString, _}
import org.scalatest.FunSuite

class MagmaTest extends FunSuite{

  test("test from gost") {
    // Create byte arrays of key and data
    val key = mapFromIntArrayToByteArray(KEY)
    val text = mapFromIntArrayToByteArray(OPEN_DATA)

    // encrypted tuple with byte data and hex view (Array: Byte, String)
    val enc = {
      val res = Magma encipher (key, text)
      (res, byteArrayToHexString(res))
    }

    // assert, that hex data equals
    assert(enc._2 == intArrToHex(ENCRYPTED_DATA))

    // decrypted tuple with byte data and hex view (Array: Byte, String)
    val dec = {
      val res = Magma decipher (key, enc _1)
      (res, byteArrayToHexString(res))
    }

    // assert, that hex data equals
    assert(dec._2 == intArrToHex(OPEN_DATA))
  }

  test("wrong key length") {
    val ex = intercept[IllegalArgumentException] {
      Magma encipher ("key", "text")
    }
    assert(ex.getMessage.contains("key must be 64 bytes"))
  }

  test("cyrillic test") {
    // Create byte arrays of key and data
    val key = mapFromIntArrayToByteArray(KEY)
    val text = CYRILLIC_OPEN_DATA

    // encrypted tuple with byte data and hex view (Array: Byte, String)
    val enc = {
      val res = Magma encipher (key, text)
      (res, byteArrayToHexString(res))
    }

    // decrypted tuple with byte data and hex view (Array: Byte, String)
    val dec = {
      val res = Magma decipher (key, enc _1)
      (res, byteArrayToHexString(res))
    }

    // assert, that text of decrypted data is equals to original text
    assert(new String(dec._1) == text)
  }
}

object MagmaTest {
  // DATA FROM GOST Appendix 2
  val KEY = Array(0xffeeddcc, 0xbbaa9988, 0x77665544, 0x33221100, 0xf0f1f2f3, 0xf4f5f6f7, 0xf8f9fafb, 0xfcfdfeff)
  val OPEN_DATA = Array(0xfedcba98, 0x76543210)
  val ENCRYPTED_DATA = Array(0x4ee901e5, 0xc2d8ca3d)

  val CYRILLIC_OPEN_DATA = "Привет, это простой тест алгоритма"

  def intArrToHex(arr: Array[Int]): String = {
    arr.map("0x" + _.toHexString).mkString(" ")
  }
}
