package base32

object Base32 extends Encoder with Decoder {
  val table = List(
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '2', '3', '4', '5',
    '6', '7'
  )
  def encode(str: String): String = {
    var res = ""
    var binary = str.getBytes
      .map(b => "%8s".format(b.toBinaryString).replace(' ', '0'))
      .mkString
    while (binary != "") {
      res = res + table(
        Integer.parseInt("%-5s".format(binary.take(5)).replace(' ', '0'), 2))
      binary = binary.drop(5)
    }
    res.size % 8 match {
      case 0 => res
      case _ => res.padTo(res.size + (8 - (res.size % 8)), pad)
    }
  }

  def decode(str: String): String = {
    var res = ""
    var binary = str
      .filter(ch => ch != '=')
      .map(c => table.indexOf(c))
      .map(i => i.toBinaryString)
      .map(s => "%5s".format(s))
      .map(s => s.replace(' ', '0'))
      .mkString

    while (binary.size >= 8) {
      res = res + Integer.parseInt(binary.take(8), 2).toChar
      binary = binary.drop(8)
    }
    return res
  }
}

trait Encoder {
  val pad = '='

  def encode(str: String): String
}

trait Decoder {
  def decode(str: String): String
}
