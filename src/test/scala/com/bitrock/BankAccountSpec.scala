package com.bitrock

class BankAccountSpec extends BaseSpec {

  "Bank account" when {

    "created" should {

      "have initial balance zero" in {
        val account = Account.open
        account.balance shouldBe Balance(0)
      }

    }

    "make a deposit" should {

      "return the new balance" in {
        val account = Account.open

        val finalBalance = (for {
          account1 <- account.deposit(deposit(value = 10))
          account2 <- account1.deposit(deposit(value = 10))
        } yield account2).value.balance

        finalBalance shouldBe Balance(20)

      }
    }

  }

  "Bank Account" when {

    "make a withdrawal on empty account" should {

      "return an NoSufficientBalance" in {
        val account = Account.open
        account.withdraw(withdrawal(value = 10)) shouldBe Left(Error("Insufficient balance: 0"))
      }
    }

    "make a withdrawal smaller than balance" should {

      "return the new balance" in {

        val account = Account.open

        val finalBalance = (for {
          account1 <- account.deposit(deposit(value = 10))
          account2 <- account1.deposit(deposit(value = 15))
          account3 <- account2.withdraw(withdrawal(value = 7))
        } yield account3).value.balance

        finalBalance shouldBe Balance(18)

      }
    }

  }

  "Bank account" should {

    "allow money transfer with sufficient balance" in {

      val account1 = Account.open
      val account2 = Account.open

      val (account11, account22) = (for {
        account11 <- account1.deposit(deposit(value = 17))
        accounts  <- account11.transfer(Transfer("date", account2, 10))
      } yield (accounts._1, accounts._2)).value

      account22.balance shouldBe Balance(10)
      account11.balance shouldBe Balance(7)

    }

    "not allow money transfer with insufficient balance" in {

      val account1 = Account.open
      val account2 = Account.open

      val result = for {
        transfer <- account1.transfer(Transfer("date", account2, 120))
      } yield transfer

      result shouldBe Left(Error("Insufficient balance: 0"))

      account1.balance shouldBe Balance(0)
      account2.balance shouldBe Balance(0)

    }
  }

  "Bank Account" when {
    "is empty" should {

      "print only the header" in {
        val account = Account.open
        account.statement shouldBe Statement("""
            |date       || credit || debit || balance
            |
            |""".stripMargin)
      }

      "print account with one deposit" in {
        val account    = Account.open
        val newAccount = account.deposit(deposit(123, "24/10/1980")).value
        newAccount.statement shouldBe Statement(
          """
            |date       || credit || debit || balance
            |24/10/1980 || 123    ||       || 123
            |""".stripMargin
        )
      }

      "print account with two deposit" in {
        val account = Account.open

        val newAccount = (for {
          newAccount   <- account.deposit(deposit(123, "24/10/1980"))
          finalAccount <- newAccount.deposit(deposit(234, "25/10/1980"))
        } yield finalAccount).value

        newAccount.statement shouldBe Statement(
          """
            |date       || credit || debit || balance
            |25/10/1980 || 234    ||       || 357
            |24/10/1980 || 123    ||       || 123
            |""".stripMargin
        )
      }

      "print account with one deposit and one withdrawal" in {
        val account = Account.open

        val newAccount = (for {
          newAccount   <- account.deposit(deposit(123, "24/10/1980"))
          finalAccount <- newAccount.withdraw(withdrawal(23, "25/10/1980"))
        } yield finalAccount).value

        newAccount.statement shouldBe Statement(
          """
            |date       || credit || debit || balance
            |25/10/1980 ||        || 23    || 100
            |24/10/1980 || 123    ||       || 123
            |""".stripMargin
        )
      }

    }
  }

  def deposit(value: Long, date: String = "date"): Deposit       = Deposit(date, value)
  def withdrawal(value: Long, date: String = "date"): Withdrawal = Withdrawal(date, value)
}
