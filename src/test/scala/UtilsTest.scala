import org.scalatest.FunSuite

class UtilsTest extends FunSuite {

  test("test Utils.byteArrayToHexString") {
    val byteArr = Utils.mapFromIntArrayToByteArray(MagmaTest.OPEN_DATA)
    val stringHexData = Utils.byteArrayToHexString(byteArr)
    assert(stringHexData == MagmaTest.OPEN_DATA_AS_STRING.mkString(" "))
  }

  test("d") {
    val arr = Utils.mapFromIntArrayToByteArray(Array (0xfedcba98))

    val a = 0
  }

}
