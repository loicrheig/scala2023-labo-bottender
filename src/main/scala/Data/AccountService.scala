package Data

import scala.collection.mutable

trait AccountService:
  /**
    * Retrieve the balance of a given account
    * @param user the name of the user whose account will be retrieve
    * @return the current balance of the user
    */
  def getAccountBalance(user: String): Double

  /**
    * Add an account to the existing accounts
    * @param user the name of the user
    * @param balance the initial balance value
    */
  def addAccount(user: String, balance: Double): Unit

  /**
    * Indicate is an account exist
    * @param user the name of the user whose account is checked to exist
    * @return whether the account exists or not
    */
  def isAccountExisting(user: String): Boolean

  /**
    * Update an account by decreasing its balance.
    * @param user the name of the user whose account will be updated
    * @param amount the amount to decrease
    * @return the new balance
    */
  def purchase(user: String, amount: Double): Double

class AccountImpl extends AccountService:
  // TODO - Part 2 Step 2
  accounts: Map[String, Double]

  def getAccountBalance(user: String): Double =
    accounts.get(user) match
      case Some(balance) => balance
      case None => throw new Exception("Account not found")

  def isAccountExisting(user: String): Boolean =
    return accounts.contains(user)
    
  def addAccount(user: String, balance: Double = 30.0): Unit =
    if (isAccountExisting(user)) throw new Exception("Account already exists")
    else accounts += (user -> balance)
  
  def purchase(user: String, amount: Double): Double =
    accounts.get(user) match
      case Some(balance) =>
        if (balance < amount) throw new Exception("Not enough money")
        else
          accounts += (user -> (balance - amount))
          return balance - amount
      case None => throw new Exception("Account not found")
end AccountImpl
