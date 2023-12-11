
class Rational(x: Int, y: Int):

    private def gcd(a:Int, b: Int): Int =
        if b == 0 then a else gcd(b, a % b)

    def numerator = x / gcd(x, y)
    def denominator = y / gcd(x, y)

    def add(r: Rational) =
        Rational(
            numerator * r.denominator + r.numerator * denominator,
            denominator * r.denominator
        )

    def multiply(r: Rational) =
        Rational(
            numerator * r.numerator,
            denominator * r.denominator
        )

    def neg() =
        Rational(-numerator, denominator)

    def subtract(r: Rational) = add(r.neg())

    def less(that: Rational) =
        numerator * that.denominator < that.numerator * denominator

    def max(that: Rational) =
        if this.less(that) then that else this

    override def toString(): String  =  s"${numerator}/${denominator}"

end Rational

Rational(1, 2).neg()
Rational(1,2).add(Rational(1, 2))
Rational(1,2).subtract(Rational(1, 2))
Rational(1,2).less(Rational(1,1))
Rational(1,2).less(Rational(10,100))
Rational(1, 2).max(Rational(3, 4))