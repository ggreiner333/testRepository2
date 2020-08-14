import io.StdIn._
import io.Source._
import java.io.PrintWriter._

object Main extends App {
  println("Would you like the abbreviations to be in Upper, Lower, or Mixed Case?")
  val userInput = readLine()

  val source = io.Source.fromFile("statesAndAbrv.txt")
  var abbreviations = source.getLines.map(line => chooseCase(userInput, (line.substring(line.indexOf("-")+1).toArray)))

  val myWriter = new java.io.PrintWriter("Abbreviations.txt")
  abbreviations.foreach(myWriter.println(_))
  myWriter.close
  source.close

  def chooseCase(userInput : String, line : Array[Char]): Array[Char] = {
    userInput match{
      case("Lower") => (String.valueOf(line).toLowerCase).toCharArray
      case("Upper") => (String.valueOf(line).toUpperCase).toCharArray
      case("Mixed") => toMixedCase(String.valueOf(line)) .toCharArray
      case(_)       => (String.valueOf(line).toUpperCase).toCharArray
    }
  }

  def toMixedCase(abrv : String): String = {
    val randomNums = ((Math.random()*2).asInstanceOf[Int], (Math.random()*2).asInstanceOf[Int])
    val newCaps = randomNums match{
      case(0, 0) => abrv.toLowerCase
      case(0, 1) => abrv.substring(0, 1).toLowerCase + abrv.substring(abrv.length-1).toUpperCase
      case(1, 0) => abrv.substring(0, 1).toUpperCase + abrv.substring(abrv.length-1).toLowerCase
      case(1, 1) => abrv.toUpperCase
      case(_, _) => abrv
    }
    newCaps
  }
}

/*

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Testing stuff

  object Main extends App {
    var num = 0
    while(num < 100){
      (num%2, num%3, num%5) match{
        case(0,0,0) => println(num + " PogChampion")
        case(_,0,0) => println(num + " pOGChamp"   )
        case(0,_,0) => println(num + " PoGChamp"   )
        case(0,0,_) => println(num + " POgChamp"   )
        case(0,_,_) => println(num + " Pog"        )
        case(_,0,_) => println(num + " pOg"        )
        case(_,_,0) => println(num + " poG"        )
        case(_,_,_) => println(num + " WeirdChamp" )
      }
      num=num+1
    }
  } 

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 1


||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 2


||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 3

  val userInput = readLine()
  val revUserInput = userInput.reverse

  if(revUserInput.equals(userInput)){
    println("Your word is a palindrome")
  }
  else{
    println("Your word is not a palindrome")
  }

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 4

  var input         = readInt()
  val originalInput = input
  var numLeapYears  = 0
  val magnitudes    = Array(60,60,24,365)
  var result        = Array(0,0,0,0,0)
  var units         = Array(" Seconds", " Minutes", " Hours", " Days", " Years")
  var masterStr     = originalInput + " Seconds is equal to "

  var i = 0
  while(i < magnitudes.length && input != 0){
    result(i) = input % magnitudes(i)
    if(result(i) == 1){
      units(i) = units(i).substring(0, (units(i).length())-1)
    }
    input = input / magnitudes(i)
    if(magnitudes(i)==365 && input >= 4){
      numLeapYears = input/4
      result(i) -= numLeapYears
      if(result(i) < 0){
        if(input%4==0){
          result(i) += 366
          numLeapYears -= 1
        }
        else{
          result(i) += 365
        }
        input -= 1
      }
    }
    i += 1
  }
  result(i) = input
  if(result(i) == 1){
    units(i) = units(i).substring(0, (units(i).length())-1)
  }
  
  units = units.reverse
  result = result.reverse

  if(result.count(_ != 0) == 0){
    masterStr = masterStr + "0 Seconds... Maybe try something more interesting next time."
  }
  else{
    if(originalInput == 1){
      masterStr = originalInput + " Second is equal to "
    }
    var j = 0
    i = result.indexWhere(_ != 0)
    masterStr = masterStr + result(i) + units(i)
    if(result.count(_ != 0)> 1){
      i = result.indexWhere(_ != 0, i+1)
      j = result.lastIndexWhere(_ != 0)
      while(i != j && i != -1){
        masterStr = masterStr + ", " + result(i) + units(i)
        i = result.indexWhere(_ != 0, i+1)
      }
      masterStr = masterStr + ", and " + result(i) + units(i)
    }
    masterStr = masterStr + "."
  }

  println(masterStr)
  if(numLeapYears > 0){
    if(numLeapYears == 1){
      println(numLeapYears + " Leap Year (A Leap Year is a year with 366 days that happenss every four years) has occured during this time.")
    }
    else{
      println(numLeapYears + " Leap Years (A Leap Year is a year with 366 days that happens every four years) have occured during this time.")
    }
  }

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 5

  Range thing is weirdChamp

  println("How many packages would you like to ship?")

  val numPackages = try {
    readInt()
  } catch{
    case _: NumberFormatException => 0
  }
  var totalShippingCost = 0.0

  var myRange = 0 until numPackages

  myRange.map(n => calculateShippingCharge(n))
  
  myRange.foreach(println)

  totalShippingCost = myRange.sum

  println("Your total shipping charge is $" + totalShippingCost)

  def calculateShippingCharge(packageNumber: Int): Double = {
    println("What is the weight of package " + packageNumber + "?")
    val weight = readDouble()
    if(weight <= 2) 2.5
    else if (weight <= 6) 4.0
    else if (weight <= 10) 5.0
    else 6.75
  }

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 6

  var myIntegers = List.fill(20)(util.Random.nextInt(50))

  println("Here is the randomized list")
  println(myIntegers.mkString("[", ", ", "]"))

  println("Which method would you like to use?")
  println("1. While"    )
  println("2. For"      )
  println("3. Count"    )
  println("4. Filter"   )
  println("5. Map"      )
  println("6. Recursion")

  var userInput  = try { 
    readInt() 
  } catch {
    case _: NumberFormatException => 1
  }
  
  userInput match{
    case(1) => println(methodOne  (myIntegers))
    case(2) => println(methodTwo  (myIntegers))
    case(3) => println(methodThree(myIntegers))
    case(4) => println(methodFour (myIntegers))
    case(5) => println(methodFive (myIntegers))
    case(6) => println(methodSix  (myIntegers))
    case(_) => println("This shouldn't have happened....")
  }

  
  println()
  println("One: "   + methodOne   (myIntegers))
  println()
  println("Two: "   + methodTwo   (myIntegers))
  println()
  println("Three: " + methodThree (myIntegers))
  println()
  println("Four: "  + methodFour  (myIntegers))
  println()
  println("Five: "  + methodFive  (myIntegers))
  println()
  println("Six: "   + methodSix   (myIntegers))
  println()
  


  def methodOne(myNumbers : List[Int]): Int = {
    var total = 0
    var i     = 0
    while(i < myNumbers.length){
      if(myNumbers(i) % 2 == 0) total += 1
      i += 1
    }
    total
  }

  def methodTwo(myNumbers : List[Int]): Int = {
    var total = 0
    for(i <- 0 until myNumbers.length){
      if(myNumbers(i) % 2 == 0) total += 1
    }
    total
  }

  def methodThree(myNumbers : List[Int]): Int = {
    myNumbers.count(_ % 2 == 0)
  }

  def methodFour(myNumbers : List[Int]): Int = {
    (myNumbers.filter(_ % 2 == 0)).length
  }

  def methodFive(myNumbers : List[Int]): Int = {
    (myNumbers.length) - ((myNumbers.map(_ % 2)).sum)
  }

  def methodSix(myNumbers : List[Int]): Int = {
    if(myNumbers.length == 0) 0 
    else if(myNumbers.head % 2 == 0) 1 + methodSix(myNumbers.drop(1)) else methodSix(myNumbers.drop(1))
  }

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 7

  println("How much USD would you like to convert?")

  val amountOfUSD = readDouble()
  var masterStr   = "$" + amountOfUSD + " is worth "
  
  println("Which currency would you like to convert to?")
  println("1 - Bitcoin"        )
  println("2 - British Pound"  )
  println("3 - Canadian Dollar")
  println("4 - Chinese Yuan"   )
  println("5 - Euro"           )
  println("6 - Indian Rupee"   )
  println("7 - Mexico Peso"    )
  println("8 - Swiss Franc"    )

  val userInput = try {
    readInt()
  } catch {
    case _: NumberFormatException => 0
  }

  userInput match{
    case(1) => masterStr += usdToBitcoin       (amountOfUSD) + " Bitcoin."
    case(2) => masterStr += usdToBritishPound  (amountOfUSD) + " British Pound."
    case(3) => masterStr += usdToCanadianDollar(amountOfUSD) + " Canadian Dollar."
    case(4) => masterStr += usdToChineseYuan   (amountOfUSD) + " Chinese Yuan."
    case(5) => masterStr += usdToEuro          (amountOfUSD) + " Euro."
    case(6) => masterStr += usdToIndianRupee   (amountOfUSD) + " Indian Rupee."
    case(7) => masterStr += usdToMexicanPeso   (amountOfUSD) + " Mexico Peso."
    case(8) => masterStr += usdToSwissFranc    (amountOfUSD) + " Swiss Franc."
    case(_) => println("You have provided an invalid input.")
  }

  println(masterStr)

  //println(masterStr + usdToBitcoin       (amountOfUSD) + " Bitcoin."        )
  //println(masterStr + usdToBritishPound  (amountOfUSD) + " British Pound."  )
  //println(masterStr + usdToCanadianDollar(amountOfUSD) + " Canadian Dollar.")
  //println(masterStr + usdToChineseYuan   (amountOfUSD) + " Chinese Yuan."   )
  //println(masterStr + usdToEuro          (amountOfUSD) + " Euro."           )
  //println(masterStr + usdToIndianRupee   (amountOfUSD) + " Indian Rupee."   )
  //println(masterStr + usdToMexicanPeso   (amountOfUSD) + " Mexico Peso."    )
  //println(masterStr + usdToSwissFranc    (amountOfUSD) + " Swiss Franc."    )

  def usdToBitcoin(dollars : Double): Double = {
    dollars * 0.000088
  }
  def usdToBritishPound(dollars : Double): Double = {
    dollars * 0.76
  }
  def usdToCanadianDollar(dollars : Double): Double = {
    dollars * 1.33
  }
  def usdToChineseYuan(dollars : Double): Double = {
    dollars * 6.95
  }
  def usdToEuro(dollars : Double): Double = {
    dollars * 0.85
  }
  def usdToIndianRupee(dollars : Double): Double = {
    dollars * 74.59
  }
  def usdToMexicanPeso(dollars : Double): Double = {
    dollars * 22.29
  }
  def usdToSwissFranc(dollars : Double): Double = {
    dollars * 0.92
  }

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 8

  val fractionOne = (-5, 7)
  val fractionTwo = (6, 13)
  var result      = (0, 0)

  println()
  result = addition(fractionOne, fractionTwo)
  println()
  result = subtraction(fractionOne, fractionTwo)
  println()
  result = multiplication(fractionOne, fractionTwo)
  println()
  result = division(fractionOne, fractionTwo)
  println()

  def addition(fractOne: (Int, Int), fractTwo : (Int, Int)): (Int, Int) = {
    var result = (0,0)

    if(fractOne._2 == fractTwo._2){
      result = (fractOne._1 + fractTwo._1, fractOne._2)
    }else{
      val lcd = LCD(fractOne._2, fractTwo._2)
      result = (((fractOne._1)*(lcd/fractOne._2) + (fractTwo._1)*(lcd/fractTwo._2)), lcd)
    }
    println(fractOne + " + " + fractTwo + " = " + result)
    result = simplify(result)
    println("After simplification, we have " + result)
    result
  }

  def subtraction(fractOne: (Int, Int), fractTwo: (Int, Int)): (Int, Int) = {
    var result  = addition(fractOne, (-fractTwo._1, fractTwo._2))
    println(fractOne + " - " + fractTwo + " = " + result)
    result = simplify(result)
    println("After simplification, we have " + result)
    result
  }

  def multiplication(fractOne: (Int, Int), fractTwo: (Int, Int)): (Int, Int) = {
    var result = (((fractOne._1)*(fractTwo._1)), ((fractOne._2)*(fractTwo._2)))
    println(fractOne + " * " + fractTwo + " = " + result)
    result = simplify(result)
    println("After simplification, we have " + result)
    result
  }

  def division(fractOne: (Int, Int), fractTwo: (Int, Int)): (Int, Int) = {
    var result = multiplication((fractOne), (fractTwo._2, fractTwo._1))
    println(fractOne + " / " + fractTwo + " = " + result)
    result = simplify(result)
    println("After simplification, we have " + result)
    result
  }

  def LCD(numOne : Int, numTwo : Int): Int = {
    val greater = if(numOne > numTwo) numOne else numTwo
    var curNum  = greater
    while(!((curNum % numOne == 0) && (curNum % numTwo == 0))){
      curNum += greater
    }
    curNum
  }

  def GCF(numOne: Int, numTwo: Int): Int = {
    val smaller = if(numOne < numTwo) numOne else numTwo
    var gcf     = -1
    var curNum  =  2
    while(curNum <= smaller){
      if(numOne % curNum == 0 && numTwo % curNum ==0){
        gcf = curNum
      }
      curNum += 1
    }
    gcf
  }

  def simplify(fract : (Int, Int)): (Int, Int) = {
    val gcf    = GCF(fract._1, fract._2)
    var result = (fract._1, fract._2)
    if(gcf != -1){
      result = (result._1/gcf, result._2/gcf)
    }
    result
  }

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 9

  println()

  val nums0 = Array(1,2,3,4,5,6,7,8,9,10)
  println(nums0.mkString("[", ", ", "]"))
  println()

  val nums1 = Array.tabulate(10)(i => i+1)
  println(nums1.mkString("[", ", ", "]"))
  println()

  var nums2 = new Array[Int](10)
  for(i <- 0 until nums2.length){
    nums2(i) = i+1
  }
  println(nums2.mkString("[", ", ", "]"))
  println()

  var nums3 = new Array[Int](10)
  var j = 0
  while(j < nums3.length){
    nums3(j) = j+1
    j += 1
  }
  println(nums3.mkString("[", ", ", "]"))
  println()

  var nums4 = Array.tabulate(20)(i => i+1)
  nums4 = nums4.filterNot(_ > 10)
  println(nums4.mkString("[", ", ", "]"))
  println()

  var nums5 = Array.tabulate(20)(i => i+1)
  nums5 = nums5.filter(_ <= 10)
  println(nums5.mkString("[", ", ", "]"))
  println()

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 10

  println()

  val nums0 = List( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(nums0.mkString("(", ", ", ")"))
  println()

  var nums1 = List[Int]()
  for(j <- 10 to 1 by -1){
    nums1 ::= j
  }
  println(nums1.mkString("(", ", ", ")"))
  println()

  var nums2 = List[Int]()
  var j     = 10
  while(j > 0){
    nums2 ::= j
    j -= 1
  }
  println(nums2.mkString("(", ", ", ")"))
  println()

  var nums3 = List(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)
  nums3 = nums3.distinct
  println(nums3.mkString("(", ", ", ")"))
  println()

  var nums4 = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  nums4 = nums4.intersect(nums0)
  println(nums4.mkString("(", ", ", ")"))
  println()

  var nums5 = List(1,2,3,4)
  nums5 = nums5.union(nums0).distinct
  println(nums5.mkString("(", ", ", ")"))
  println()

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 11

  var myIntegers = Array.fill(100)(util.Random.nextInt(100))

  println("Here is the randomized array")
  println(myIntegers.mkString("[", ", ", "]"))

  println("Which method would you like to use?")
  println("1. Recursion")
  println("2. Count"    )
  println("3. Filter"   )
  println("4. Map"      )
  println("5. For"      )
  println("6. While"    )

  var userInput  = try { 
    readInt() 
  } catch {
    case _: NumberFormatException => 1
  }
  
  userInput match{
    case(1) => println(methodOne  (myIntegers))
    case(2) => println(methodTwo  (myIntegers))
    case(3) => println(methodThree(myIntegers))
    case(4) => println(methodFour (myIntegers))
    case(5) => println(methodFive (myIntegers))
    case(6) => println(methodSix  (myIntegers))
    case(_) => println("This shouldn't have happened....")
  }
  
  /*
  println()
  println("One: " + methodOne  (myIntegers))
  println()
  println("Two: " + methodTwo  (myIntegers))
  println()
  println("Three: " + methodThree  (myIntegers))
  println()
  println("Four: " + methodFour  (myIntegers))
  println()
  println("Five: " + methodFive  (myIntegers))
  println()
  println("Six: " + methodSix  (myIntegers))
  println()
  */


  def methodOne(myNumbers : Array[Int]): Int = {
    if(myNumbers.length == 0) 0 
    else if(myNumbers.head % 2 == 0) 1 + methodOne(myNumbers.drop(1)) else methodOne(myNumbers.drop(1))
  }

  def methodTwo(myNumbers : Array[Int]): Int = {
    myNumbers.count(_ % 2 == 0)
  }

  def methodThree(myNumbers : Array[Int]): Int = {
    (myNumbers.filter(_ % 2 == 0)).length
  }

  def methodFour(myNumbers : Array[Int]): Int = {
    (myNumbers.length) - ((myNumbers.map(_ % 2)).sum)
  }

  def methodFive(myNumbers : Array[Int]): Int = {
    var i = 0
    var total = 0
    for(i <- 0 until myNumbers.length){
      if(myNumbers(i)%2 == 0){
        total += 1
      }
    }
    total
  }

  def methodSix(myNumbers : Array[Int]): Int = {
    var i = 0
    var total = 0
    while(i < myNumbers.length){
      if(myNumbers(i)%2 == 0){
        total += 1
      }
      i += 1
    }
    total
  }

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

  Exercise 12

  println("Would you like the abbreviations to be in Upper, Lower, or Mixed Case?")
  val userInput = readLine()

  val source = io.Source.fromFile("statesAndAbrv.txt")
  var abbreviations = source.getLines.map(line => chooseCase(userInput, (line.substring(line.indexOf("-")+1).toArray)))

  val myWriter = new java.io.PrintWriter("Abbreviations.txt")
  abbreviations.foreach(myWriter.println(_))
  myWriter.close
  source.close

  def chooseCase(userInput : String, line : Array[Char]): Array[Char] = {
    userInput match{
      case("Lower") => (String.valueOf(line).toLowerCase).toCharArray
      case("Upper") => (String.valueOf(line).toUpperCase).toCharArray
      case("Mixed") => toMixedCase(String.valueOf(line)) .toCharArray
      case(_)       => (String.valueOf(line).toUpperCase).toCharArray
    }
  }

  def toMixedCase(abrv : String): String = {
    val randomNums = ((Math.random()*2).asInstanceOf[Int], (Math.random()*2).asInstanceOf[Int])
    val newCaps = randomNums match{
      case(0, 0) => abrv.toLowerCase
      case(0, 1) => abrv.substring(0, 1).toLowerCase + abrv.substring(abrv.length-1).toUpperCase
      case(1, 0) => abrv.substring(0, 1).toUpperCase + abrv.substring(abrv.length-1).toLowerCase
      case(1, 1) => abrv.toUpperCase
      case(_, _) => abrv
    }
    newCaps
  }

*/

