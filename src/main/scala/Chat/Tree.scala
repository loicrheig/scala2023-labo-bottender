package Chat

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree
sealed trait Command extends ExprTree
sealed trait BasicCommand extends Command

//todo peut etre tous des ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  // Example cases
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree

  case object Solde extends ExprTree
  case class Auth(username: String) extends ExprTree
  case class Product(quantity: Int, productName: String, productBrand: Option[String])

  case class BasicOrder(product: Product, isRealOrder: Boolean) extends ExprTree
  case class AndOrder(leftCommand: ExprTree, rightCommand: BasicOrder, isRealOrder: Boolean) extends ExprTree
  case class OrOrder(leftCommand: ExprTree, rightCommand: BasicOrder, isRealOrder: Boolean) extends ExprTree

  /*
  case object Command extends ExprTree
  case class BasicOrder(product: Product) extends Command
  case class AndOrder(leftCommand: BasicOrder, rightCommand: Command) extends Command
  case class OrOrder(leftCommand: BasicOrder, rightCommand: Command) extends Command*/
