package FunctionScala

object bankAccounts {

class Account(id:String,b:Double,n:Int)
    {

      val nic:String=id
      val acnumber:Int=n
      var balance: Double = b

      override def toString: String ="["+nic +":"+ balance +":"+acnumber+"]"

      //withraw
      def withdraw(a:Double): Unit = {this.balance=this.balance-a}
      //deposit
      def deposit(a:Double): Unit = {this.balance=this.balance+a}

        //Qusetion 3 //transfer
      def transfer(a:Account,b:Double): Unit ={this.withdraw(b)
        a.deposit(b)
      }
 }
//Question 4

  //overdraft balance accounts //Question 4.1
  val overdraft: List[Account] => List[Account] = (b:List[Account])=> b.filter(x=>x.balance<0)
  //sum of all the balance accounts
  // Question 4.2
  val sum= ( b: List[Account] ) => b.map( x => (x,x.balance) ).reduce( (a , c) => ( c._1 , a._2 + c._2) )

  // Interest //Question 4.3
val interest = (b:List[Account])=>b.map((x) => (x.nic,x.acnumber,if(x.balance>0)  (x.balance+(x.balance*0.05)) else (x.balance+(x.balance*0.1)) ))


  def main(args: Array[String]): Unit = {

      val a = new Account("987900075v", 12000, 1)
      val b = new Account("9879sft075v", 2000, 2)
      val c = new Account("9782728827v", -100, 3)
      val d = new Account("978277888v", 99800, 4)
      val bank: List[Account] = List(a,b,c,d)
println(" ")
        println("All the accounts" + bank)
    println(" ")
      println("accounts which  have negative balance :" +overdraft(bank))
    println(" ")

      println("balance of all accounts " + sum( bank )._2 )
    println(" ")

       a.transfer(b, 900)
      println("After transfer RS.900 to account b from a ")
      println(bank(0))
      println(bank(1))
println("")
    println("After Applying Interest")
println(interest(bank))



    }






}


