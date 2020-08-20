import io.StdIn._
import io.Source._
import java.io.PrintWriter._
import java.util._

object RPN {
    var ON      = true
    var myStack = new Stack[Double]
    var userInput = ""

    def runner(){
        while(ON){
            userInput = readLine()
            fillSpace(25)

            userInput match{
                case("+") => add(myStack)
                case("-") => subtract(myStack)
                case("*") => multiply(myStack)
                case("/") => divide(myStack)
                case(_)   => actOnInput(userInput, myStack)
            }

            printStack(myStack)
        }
    }
  
    def add(stack : Stack[Double]){
        if(stack.size() < 2){
            println("Not enough elements")
        }else{
            stack.push(stack.pop() + stack.pop())
        }
    }

    def subtract(stack : Stack[Double]){
        if(stack.size() < 2){
            println("Not enough elements")
        }else{
            stack.push(-stack.pop() + stack.pop())
        }
    }

    def multiply(stack : Stack[Double]){
        if(stack.size() < 2){
            println("Not enough elements")
        }else{
            stack.push(stack.pop() * stack.pop())
        }
    }

    def divide(stack : Stack[Double]){
        if(stack.size() < 2){
            println("Not enough elements")
        }else{
            stack.push((1/stack.pop()) * stack.pop())
        }
    }
    
    def actOnInput(input : String, stack : Stack[Double]){
        if(isDigitInput(input)){
            stack.push(input.toDouble)
        }else if(input.equals("quit")){
            ON = false
        }else{
            println("invalid input")
        }
    }
    
    def isDigitInput(input : String) : Boolean = {
        var curChar = ' '
        val chars   = input.toCharArray()
        var i       = 0

        for(i <- 0 until chars.length){
            curChar = input(i)
            if(!(curChar.isDigit) & !(curChar.equals('.')) & !(curChar.equals('-'))){
                return false
            }
        }

        true
    }


    def printStack(stack : Stack[Double]){
        var i = 0
        val myIt = stack.iterator()

        println("Stack")
        while(myIt.hasNext()){
            printf("%d : %.2f%n", i, myIt.next())
            i+=1
        }
        println()
    }

    def fillSpace(numLines : Int) {
        var i = 0
        for(i <- 0 until numLines){
            println()
        }
    }
}
