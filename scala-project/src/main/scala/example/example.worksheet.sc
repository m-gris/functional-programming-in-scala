5 + 1

val x = 42

x * x * x

def abs(x: Double):Double =
    if (x < 0) then -x else x


def sqrt(x: Double):Double =

    def isGoodEnough(guess: Double) =
        abs(x - guess * guess) < 0.0001

    def improve(guess: Double) =
        (guess + x / guess) / 2

    def sqrtIter(guess: Double): Double =
        if (isGoodEnough(guess)) guess
        else sqrtIter(improve(guess))

    sqrtIter(1.0)

sqrt(25)

def id(x: Int) = x
def cube(x: Int): Int = x * x * x
def factorial(x: Int): Int = if x < 2 then 1 else x * factorial(x -1)

def summation(f: Int => Int, a: Int, b: Int):Int =
    if a > b then 0 else f(a) + summation(f, a+1, b)

def sumInts(a:Int, b:Int):Int = summation(id, a, b)
def sumCubes(a:Int, b:Int):Int = summation(cube, a, b)
def sumFact(a:Int, b:Int):Int = summation(factorial, a, b)

sumInts(1, 3)
sumCubes(1, 3)
sumFact(1, 6)

// with ANONYMOUS FUNCTIONS

def sumIntsLambda(a:Int, b:Int):Int = summation( ((x:Int) => x), a, b)
def sumCubesLambda(a:Int, b:Int):Int = summation((x => x*x*x), a, b) // type was infered

sumIntsLambda(1, 3)
sumCubes(1, 3)
sumFact(1, 6)

// TAIL RECURSIVE VERSION
import scala.annotation.tailrec
def summationTail(f: Int=>Int, a:Int, b:Int):Int =
    @tailrec // ensures that the compiler will perform tail call optimization.
    def loop(a: Int, acc:Int):Int =
        if a > b then acc
        else loop(acc + f(a), a + 1)
    loop(0, a)

def summationCurried(f:Int=>Int) =
    def sumF(a:Int, b:Int):Int =
        if a > b then 0 else f(a) + sumF(a+1, b)
    sumF

summationCurried(cube)(1, 3)

def summationCurriedBetter(f:Int=>Int)(a: Int, b: Int): Int =
    if a > b then 0 else f(a) + summationCurriedBetter(f)(a+1, b)

summationCurriedBetter(cube)(1, 3)


def product(f: Int => Int)(a: Int, b: Int): Int=
    if a > b then 1 else f(a) * product(f)(a+1, b)

product(id)(1, 5)

def fact(n:Int):Int = product(id)(1, n)

fact(5)


// LET'S ABSTRACT summation & product

def mapReduce(f: Int=> Int, // the function to be mapped
              loi_de_composition_interne: (Int, Int) => Int, // reduce func
              element_neutre: Int  // the 'thing' to return when the interval is empty
            )
            // the returned function
            // takes our interval of bounds from a to b
            (a: Int, b:Int): Int =

    // helper function
    def recur(a:Int):Int =
        if a > b
            then element_neutre
            else loi_de_composition_interne(f(a), recur(a+1))
    recur(a)


def abstractSummation(f: Int=>Int) = mapReduce(f, (x, y) => x + y, 0)

abstractSummation(fact)(1, 5)


def abstractProduct(f: Int=>Int) = mapReduce(f, (x, y) => x * y, 1)

abstractProduct(fact)(1, 5)


// FINDING FIXED POINTS, i.e   f(x) = x
// for many functions, such points can be found by
// starting with an estimate
// then repetetively apply the function
// until the output is stable or "stable enough"

val tolerance = 0.0001

def isCloseEnough(x:Double, y:Double): Boolean =
    abs((x -y) / x) < tolerance

def fixedPoint(f: Double=>Double)(firstGuess: Double):Double =
    def iterate(guess:Double):Double =
        val next = f(guess)
        if isCloseEnough(guess, next)
            then next
            else iterate(next)
    iterate(firstGuess)

// stabilizing by averaging
def averageDamp(f: Double => Double)(x: Double): Double =
    (x + f(x)) / 2

def squareRoot(x: Double): Double = fixedPoint (averageDamp(y => x / y)) (1.0)




