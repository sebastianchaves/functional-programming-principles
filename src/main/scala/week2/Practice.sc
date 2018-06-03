def sumInts = sumCurry(x => x)
def sumCubes = sumCurry(x => x*x*x)
def sumFactorials = sumCurry(factorial)

def factorial(x: Int): Int = if (x == 0) 1 else x * factorial(x - 1)

def sumCurry(f: Int => Int): (Int, Int) => Int = {
  def sumCurryF(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }
  sumCurryF
}

sumInts(7, 6)
sumCubes(2, 4)
sumFactorials(3, 4)

def sumCheta(f: Int => Int)(a: Int, b: Int): Int =
  if(a > b) 0 else f(a) + sumCheta(f)(a + 1, b)

@Deprecated
def sumOf(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sumOf(f, a + 1, b)

@Deprecated
def sum(f: Int => Int, a: Int, b: Int): Int = {

  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }

  loop(a, 0)

}