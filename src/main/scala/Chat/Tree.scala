package Chat

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  // Example cases
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree

  case class Auth(username: String) extends ExprTree

  case class Product(quantity: Int, productName: String, productBrand: Option[String]) extends ExprTree
  
  case object Command extends ExprTree
  case object BasicCommand extends Command

  case class BasicOrder(product: Product) extends BasicCommand
  case class BasicPriceAsk(product: Product) extends BasicCommand
  case class AndOrder(leftCommand: BasicCommand, rightCommand: Command) extends Command
  case class OrOrder(leftCommand: BasicCommand, rightCommand: Command) extends Command
