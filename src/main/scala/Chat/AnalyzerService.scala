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
      case Order(n Num p ProductName) => n * productSvc.getPrice(p)
      case Order(leftOrder Order And rightOrder Order) => computePrice(leftOrder) + computePrice(rightOrder)
    

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
        if accountSvc.isAccountExisting(user) == false{
          accountSvc.addAccount(user)
        }
        session.setCurrentUser(user)
      case OrOrder(leftCommand, rightCommand) =>
        val leftPrice = computePrice(leftCommand)
        val rightPrice = computePrice(rightCommand)

        if leftPrice <= rightPrice{
          inner(leftCommand)
        }
        else{
          inner(rightCommand)
        }
      case AndOrder(leftCommand, rightCommand) =>
        inner(leftCommand) + " et " inner(rightCommand)

      case BasicOrder(product) =>
        val brand = ""

        product.productBrand match
          case Some(s) => brand = s
          case None => brand = productSvc.getDefaultBrand(product.productName)
        
        val productPrice = computePrice(t)
        accountSvc.purchase(session.getCurrentUser(), productPrice)

        return "Voici donc " + product.quantity.toString() + " " + brand + " ! Cela coûte CHF " + productPrice.toString() + " et votre nouveau solde est de " accountSvc.getAccountBalance(session.getCurrentUser())
        
end AnalyzerService
