object Rational{
  class Rational(n:Int,d:Int)
  {
    def numer=n/gcd(Math.abs(n),d)
    def denom=d/gcd(Math.abs(n),d)

    private def gcd(a:Int, b:Int) :Int= if(b==0) a else gcd(b,a%b)

    //Question 01 negation of Rational Number
    def neg=new Rational(-this.numer,this.denom)


    //Qusetion 2 substraction
    def +(r:Rational)=new Rational(this.numer * r.denom + r.numer * this.denom, this.denom * r.denom)
    def -(r:Rational)= this+r.neg


    override def toString: String =numer+"/"+denom


  }
  def main(args: Array[String]): Unit = {

    var x = new Rational(3, 4)
    var y = new Rational(5, 8)
    var z = new Rational(2, 7)
    println("x="+x)
    println("y="+y)
    println("z="+z)

    println("Negation of" + " " + x + " " + "is " + " " + x.neg)
    val a = x - y - z
    println("Results:x-y-z=" + a)
  }
}
