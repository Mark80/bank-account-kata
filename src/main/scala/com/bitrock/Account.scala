package com.bitrock

sealed trait Movement {
  def amount: Long
  def date: String
}

case class Deposit(date: String, amount: Long)                          extends Movement
case class Withdrawal(date: String, amount: Long)                       extends Movement
case class Transfer(date: String, targetAccount: Account, amount: Long) extends Movement

case class Balance(value: Long) {

  def +(movement: Movement): Balance = movement match {
    case Deposit(_, amount)    => Balance(value + amount)
    case Withdrawal(_, amount) => Balance(value - amount)
  }

  def isInsufficient: Boolean = value < 0
}

object Balance {
  val Zero: Balance = Balance(0)
}

case class Error(message: String)

case class MovementsLog(movements: List[Movement]) {
  def addMovement(movement: Movement): MovementsLog =
    MovementsLog(movement :: movements)
}

object MovementsLog {
  def withNoMovements(): MovementsLog =
    MovementsLog(Nil)
}

case class Account(movementsLog: MovementsLog) {

  type OperationResult[T] = Either[Error, T]

  def balance: Balance = {
    val movements = movementsLog.movements
    movements.foldLeft(Balance.Zero) { (balance, movement) =>
      balance + movement
    }
  }

  def transfer(transfer: Transfer): OperationResult[(Account, Account)] = {
    import transfer.targetAccount
    for {
      account      <- withdraw(Withdrawal(transfer.date, transfer.amount))
      otherAccount <- targetAccount.deposit(Deposit(transfer.date, transfer.amount))
    } yield (account, otherAccount)
  }

  def withdraw(withdrawal: Withdrawal): OperationResult[Account] =
    balance + withdrawal match {
      case money if money.isInsufficient =>
        insufficientMoneyError(balance.value)
      case _ =>
        addMovement(withdrawal)
    }

  def deposit(deposit: Deposit): OperationResult[Account] =
    addMovement(deposit)

  private def insufficientMoneyError(money: Long): Left[Error, Nothing] =
    Left(Error(s"Insufficient balance: $money"))

  private def addMovement(movement: Movement): OperationResult[Account] =
    Right(Account(movementsLog.addMovement(movement)))
}

object Account {

  def open: Account = Account(MovementsLog.withNoMovements())

}
