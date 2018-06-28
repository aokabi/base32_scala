package base32

import org.scalatest._

class Base32EncodeSpec extends FlatSpec {
  "String \"ABC\"" should "encode \"IFBEG===\"" in {
    assert(Base32.encode("ABC") == "IFBEG===")
  }
  "String \"()^[]:.,#\"{}*+`@$%&()\"" should "encode \"FAUV4W25HIXCYIZCPN6SUK3AIASCKJRIFE======\"" in {
    val result = Base32.encode("()^[]:.,#\"{}*+`@$%&()")
    assert(result == "FAUV4W25HIXCYIZCPN6SUK3AIASCKJRIFE======")
  }
  "String \"あ\"" should "encode \"4OAYE===\"" in {
    val result = Base32.encode("あ")
    assert(result == "4OAYE===")
  }
}

class Base32DecodeSpec extends FlatSpec {
  "String \"IFBEG===\"" should "decode \"ABC\"" in {
    assert(Base32.decode("IFBEG===") == "ABC")
  }
  "String \"FAUV4W25HIXCYIZCPN6SUK3AIASCKJRIFE======\"" should "decode \"()^[]:.,#\"{}*+`@$%&()\"" in {
    val result = Base32.decode("FAUV4W25HIXCYIZCPN6SUK3AIASCKJRIFE======")
    assert(result == "()^[]:.,#\"{}*+`@$%&()")
  }
  "String \"4OAYE===\"" should "decode \"あ\"" in {
    val result = Base32.decode("4OAYE===")
    assert(result == "あ")
  }
}

class MapSpec extends FlatSpec {
  val table = Map[Int, Char](
    0 -> 'A',
    1 -> 'B',
    2 -> 'C',
    3 -> 'D',
    4 -> 'E',
    5 -> 'F',
    6 -> 'G',
    7 -> 'H',
    8 -> 'I',
    9 -> 'J',
    10 -> 'K',
    11 -> 'L',
    12 -> 'M',
    13 -> 'N',
    14 -> 'O',
    15 -> 'P',
    16 -> 'Q',
    17 -> 'R',
    18 -> 'S',
    19 -> 'T',
    20 -> 'U',
    21 -> 'V',
    22 -> 'W',
    23 -> 'X',
    24 -> 'Y',
    25 -> 'Z',
    26 -> '2',
    27 -> '3',
    28 -> '4',
    29 -> '5',
    30 -> '6',
    31 -> '7'
  )
  "table(1)" should "is '7'" in {
    assert(table(Integer.parseInt("11111", 2)) == '7')
  }
}
