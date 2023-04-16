package Chat
import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):
  import ExprTree._
  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    * @return the result of the computation
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): Double = 
    t match
      case BasicOrder(product) =>
        val brand = product.productBrand.getOrElse("")
        product.quantity * productSvc.getPrice(product.productName, brand)
      case AskPrice(product) => 
        val brand = product.productBrand.getOrElse("")
        product.quantity * productSvc.getPrice(product.productName, brand)
      case AndOrder(leftCommand, rightCommand) => computePrice(leftCommand) + computePrice(rightCommand)
      case OrOrder(leftCommand, rightCommand) => {
        val leftPrice = computePrice(leftCommand)
        val rightPrice = computePrice(rightCommand)

        if leftPrice <= rightPrice then {
          leftPrice
        }
        else{
          rightPrice
        }
      }
      case _ => 0.0
  
  def handleOrder(t: ExprTree, session: Session): Double = 
    val price = computePrice(t)
    session.getCurrentUser match
      case None => return 0.0
      case Some(user) => accountSvc.purchase(user, price)
    price

  /**
    * Return the output text of the current node, in order to write it in console.
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    t match
      // TODO - Part 2 Step 3
      // Example cases
      case Thirsty => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case Auth(username) => 
        if (accountSvc.isAccountExisting(username) == false){
          // todo remove 30.0
          accountSvc.addAccount(username, 30.0)
        }
        session.setCurrentUser(username)
        "Bonjour " + username + " !"
      case AskPrice(product) => 
        val quantity = product.quantity.toString()
        val productName = product.productName
        val price = computePrice(t).toString()
        val brand = product.productBrand.getOrElse("Pas de marques")
        s"Le prix de $quantity $productName $brand est de CHF $price !"
      case Solde => 
        session.getCurrentUser match
          case None => "Vous n'êtes pas connecté !"
          case Some(user) => 
            val balance = accountSvc.getAccountBalance(user).toString()
            s"Votre solde est de CHF $balance !"
      case OrOrder(leftCommand, rightCommand) =>
        session.getCurrentUser match
          case None => "Vous n'êtes pas connecté !"
          case Some(user) => 
            val price = handleOrder(t, session).toString()
            s"Le prix le plus bas est de CHF $price !"
      case AndOrder(leftCommand, rightCommand) =>
        session.getCurrentUser match
          case None => "Vous n'êtes pas connecté !"
          case Some(user) => 
            val price = handleOrder(t, session)
            val balance = accountSvc.getAccountBalance(user).toString()
            s"Le prix total est de CHF $price et votre nouveau solde est de $balance !"
      case BasicOrder(product) =>
        session.getCurrentUser match
          case None => "Vous n'êtes pas connecté !"
          case Some(user) => 
            val price = handleOrder(t, session)
            val balance = accountSvc.getAccountBalance(user).toString()
            val quantity = product.quantity.toString()
            val brand = product.productBrand.getOrElse("Pas de marques")
            val tp = product.productName
            s"Voici donc $quantity $tp $brand ! Cela coûte CHF $price et votre nouveau solde est de $balance !"
        
end AnalyzerService
