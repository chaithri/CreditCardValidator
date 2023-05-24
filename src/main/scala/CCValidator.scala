import scala.io.StdIn.readLine
object CCValidator {

  /** cardProcessor method is to validate the length of card number and to
   * identify the Processor company
   */
  private def cardProcessor(str: String): String = {

    var firstNum: Int = str.charAt(0).asDigit
    if (firstNum == 3)
      firstNum = str.substring(0, 2).toInt
    if (str.length() < 13 || str.length() > 16) {
      println(
        "The entered credit card number is invalid as the length is not in between 13 and 16"
      )
      System.exit(0)
    }
    val msg = firstNum match {
      case 4 => "Visa"
      case 5 => "MasterCard"
      case 6 => "Discover"
      case 37 => "American Express"
      case _ => "Invalid"
    }
    msg
  }

  private def digitsSum(i: Int): Int = {
    var s: Int = 0
    var n = i
    while (n > 0) {
      s = s + (n % 10)
      n = n / 10
    }
    s
  }

  /** Implementation of Luhn Algorithm to validate credit card number
   */
  private def cardValidator(str: String): String = {
    val numList = str.reverse.split("").toList.map(_.toInt)
    println(numList)
    val stagingList = numList.zipWithIndex.map { case (x, i) =>
      if (i % 2 == 1) x * 2 else x
    }
    println(stagingList)
    val res: Int = {
      stagingList.reduce((a, b) => a + digitsSum(b))
    }
    println(res)
    if (res % 10 == 0) "Valid" else "Invalid"
  }

  def main(args: Array[String]): Unit = {

    println("Enter Card Number: ")
    val input = readLine(); //"a 4408 @0412 ,.3456 $7893"
    val str = input.replaceAll("[^0-9]", "")
    println("Given Card Number is " + str)
    val processor = cardProcessor(str)
    if (processor.equals("Invalid"))
      println("The card number " + str + " is " + processor + "as it doesn't start with 37/4/5/6")
    else
      println(
        "The entered card number belongs to the card Processor : " + processor + " and it is " + cardValidator(
          str
        )
      )

  }
}

