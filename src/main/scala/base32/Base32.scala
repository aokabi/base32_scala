package base32

object Base32 extends Encoder with Decoder {
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
  def encode(str: String): String = {
    var res = ""
    var binary = str.getBytes
      .map(b => "%8s".format(b.toBinaryString).replace(' ', '0'))
      .mkString
    while (binary != "") {
      res = res + table(Integer.parseInt("%-5s".format(binary.take(5)).replace(' ', '0'), 2))
      binary = binary.drop(5)
    }
    res.size % 8 match {
      case 0 => res
      case _ => res.padTo(res.size + (8- (res.size%8)), pad)
    }
  }

  def decode(): String = ""
}

trait Encoder {
  val pad = '='

  def encode(str: String): String
}

trait Decoder {
  def decode(): String
}
