package com.bitrock

class BankAccountSpec extends BaseSpec {


  "Bank account" when {

    "created" should {

      "have initial balance zero" in {
        val account = new Account()
        account.balance shouldBe Balance(0)
      }

    }

    "make a deposit" should {

      "return the new balance" in {
        val account = new Account()
        account.deposit(Deposit(10))
        account.deposit(Deposit(15))
        account.balance shouldBe Balance(25)
      }
    }

  }

  "Bank Account" when {

    "make a withdrawal on empty account" should {

      "return an NoSufficientBalance" in {
        val account = new Account()
        account.withdraw(Withdrawal(10)) shouldBe Left(Error("Insufficient balance: 0"))
      }
    }

    "make a withdrawal on smaller than balance" should {

      "return the new balance" in {

        val account = new Account()

        for {
          _ <- account.deposit(Deposit(10))
          _ <- account.deposit(Deposit(15))
          _ <- account.withdraw(Withdrawal(7))
        } yield ()

        account.balance shouldBe Balance(18)
      }
    }

  }

  "Bank account" should {

    "allow money transfer with sufficient balance" in {

      val account1 = new Account()
      val account2 = new Account()

      account1.deposit(Deposit(17))
      account1.transfer(account2, Transfer(10))

      account2.balance shouldBe Balance(10)
      account1.balance shouldBe Balance(7)

    }

    "not allow money transfer with insufficient balance" in {

      val account1 = new Account()
      val account2 = new Account()

      account1.deposit(Deposit(17))
      val result = account1.transfer(account2, Transfer(120))

      result shouldBe Left(Error("Insufficient balance: 17"))
      account2.balance shouldBe Balance(0)
      account1.balance shouldBe Balance(17)

    }

  }


}

case class Deposit(amount: Long)

case class Balance(value: Long)

case class Withdrawal(value: Long)

case class Transfer(value: Long)

case class Error(message: String)

class Account() {

  var balance: Balance = Balance(0)

  def transfer(account2: Account, transfer: Transfer): Either[Error, Unit] = {
    for {
      _ <- withdraw(Withdrawal(transfer.value))
      _ <- account2.deposit(Deposit(transfer.value))
    } yield ()
  }

  def withdraw(value: Withdrawal): Either[Error, Unit] = {
    val newBalance = balance.value - value.value
    newBalance match {
      case value if value < 0 => Left(Error(s"Insufficient balance: ${balance.value}"))
      case _ =>
        balance = Balance(newBalance)
        Right(())
    }
  }

  def deposit(deposit: Deposit): Either[Error, Unit] = {
    balance = balance.copy(value = balance.value + deposit.amount)
    Right(())
  }


}


