package com.bitrock

sealed trait Movement {
  def amount: Long
  def date: String
}

case class Deposit(date: String, amount: Long)    extends Movement
case class Withdrawal(date: String, amount: Long) extends Movement
case class Transfer(date: String, targetAccount: Account, amount: Long)
case class Statement(st: String)
object Statement {

  def build(movements: List[Movement]): Statement = {
    val (statementRows, _) = movements.reverse.foldLeft((List.empty[StatementRow], Balance.Zero)) {
      case ((statementRows, previousBalance), movement) =>
        val currentBalance = previousBalance + movement
        (StatementRow(movement, currentBalance.value) :: statementRows, currentBalance)
    }
    Statement(s"""
                 |date       || credit || debit || balance
                 |${statementRows.map(_.row).mkString("\n")}
                 |""".stripMargin)
  }
}

case class StatementRow(row: String)

object StatementRow {

  def apply(mov: Movement, currentBalance: Long): StatementRow = mov match {
    case Deposit(date, amount) =>
      StatementRow(date + " || " + amount + "    ||       || " + currentBalance)
    case Withdrawal(date, amount) =>
      StatementRow(date + " ||        || " + amount + "    || " + currentBalance)

  }

}

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
  def empty(): MovementsLog =
    MovementsLog(Nil)
}

case class Account(movementsLog: MovementsLog) {

  type OperationResult[T] = Either[Error, T]

  lazy val balance: Balance =
    sumAllMovements(movementsLog.movements)

  private def sumAllMovements(movements: List[Movement]): Balance =
    movements.foldLeft(Balance.Zero) { (balance, movement) =>
      balance + movement
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

  lazy val statement: Statement = Statement.build(movementsLog.movements)

}

object Account {

  def open: Account = Account(MovementsLog.empty())

}
