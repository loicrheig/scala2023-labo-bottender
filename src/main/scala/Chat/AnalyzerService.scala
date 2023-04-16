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
      case BasicOrder(product) => product.quantity * productSvc.getPrice(product.productName, product.productBrand)
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
      case AskPrice(product) => "Le prix de "  product.quantity.toString() + " " + product.productName + " est de CHF " + computePrice(BasicOrder(product)).toString()
      case Solde => "Votre solde est de CHF " + accountSvc.getAccountBalance(session.getCurrentUser()).toString()
      case OrOrder(leftCommand, rightCommand) =>
        val price = handleOrder(t, session)

        return "Le prix le plus bas est de CHF " + price.toString() + " !"
      case AndOrder(leftCommand, rightCommand) =>
        val price = handleOrder(t, session)

        return "Le prix total est de CHF " + price.toString() + " !"

      case BasicOrder(product) =>
        val price = handleOrder(t, session)

        return "Voici donc " + product.quantity.toString() + " " + product.productBrand.getOrElse("Pas de marques") + " ! Cela coûte CHF " + price.toString() + " et votre nouveau solde est de " accountSvc.getAccountBalance(session.getCurrentUser())
        
end AnalyzerService
