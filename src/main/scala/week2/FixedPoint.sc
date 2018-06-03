import scala.math.abs

val tolerance = 0.0001

def isCloseEnough(guess: Double, next: Double) = abs((guess - next) / guess) < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {

  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

fixedPoint(x => 1 + x/2)(1)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2

def sqrt(n: Double): Double = fixedPoint(averageDamp(y => n / y))(1.0)

sqrt(2)
