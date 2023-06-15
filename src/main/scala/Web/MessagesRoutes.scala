package Web

import Chat.{AnalyzerService, TokenizerService}
import Data.{MessageService, AccountService, SessionService, Session}
import scalatags.Text.all._
import scalatags.Text.tags2
import Layouts.basicPage
import Layouts.messageList
import Layouts.message
import Chat.Parser
import Chat.ExprTree.*
import Chat.ExprTree
import castor.Context.Simple.global
import scala.collection.mutable.ListBuffer
import Chat.UnexpectedTokenException
import Utils.FutureOps.randomSchedule
import scala.concurrent.duration.*
import scala.concurrent.Future
import org.scalactic.Bool



/** Assembles the routes dealing with the message board:
  *   - One route to display the home page
  *   - One route to send the new messages as JSON
  *   - One route to subscribe with websocket to new messages
  *
  * @param log
  */
class MessagesRoutes(
    tokenizerSvc: TokenizerService,
    analyzerSvc: AnalyzerService,
    msgSvc: MessageService,
    accountSvc: AccountService,
    sessionSvc: SessionService
)(implicit val log: cask.Logger)
    extends cask.Routes:
  import Decorators.getSession

  var webSockets = ListBuffer[cask.WsChannelActor]()

  @getSession(
    sessionSvc
  ) // This decorator fills the `(session: Session)` part of the `index` method.
  @cask.get("/")
  def index()(session: Session) =
    // TODO - Part 3 Step 2: Display the home page (with the message board and the form to send new messages)
    basicPage(
      messageList(),
    )

  // TODO - Part 3 Step 4b: Process the new messages sent as JSON object to `/send`. The JSON looks
  //      like this: `{ "msg" : "The content of the message" }`.
  //
  //      A JSON object is returned. If an error occurred, it looks like this:
  //      `{ "success" : false, "err" : "An error message that will be displayed" }`.
  //      Otherwise (no error), it looks like this:
  //      `{ "success" : true, "err" : "" }`
  //
  //      The following are treated as error:
  //      - No user is logged in
  //      - The message is empty
  //
  //      If no error occurred, every other user is notified with the last 20 messages
  //

  def getProductsFromExpr(expr: ExprTree) : List[Product] = expr match {
    case And(left, right) => getProductsFromExpr(left) ++ getProductsFromExpr(right)
    case Or(left, right) => getProductsFromExpr(left) ++ getProductsFromExpr(right)
    case Product(name, brand, quantity) => List(Product(name, brand, quantity))
    case _ => List()
  }

  def synchCommands(session: Session, product: Product) : Future[(Int, String, Boolean)] = {    
    def loop(session: Session, productName: String, productQuantityLeft: Int, acc: Int, notFailed: Boolean) : Future[(Int, String, Boolean)] = {
      if (productQuantityLeft == 0) {
        Future.successful((acc, product.name, notFailed))
      } else {
        val wait = randomSchedule(3.second, 0.second, 0.4)
        wait.flatMap(_ => {
          loop(session, product.name, productQuantityLeft - 1, acc + 1, notFailed)
        }).recoverWith( { case e: Exception => loop(session, product.name, productQuantityLeft - 1, acc, false) })
      }
    }

    loop(session, product.name, product.quantity, 0, true)
  }

  @getSession(sessionSvc)
  @cask.postJson("/send")
  def send(msg: String)(session: Session) : ujson.Obj =
    // TODO - Part 3 Step 2: Display the home page (with the message board and the form to send new messages)
    // show a message in the console
    // return a JSON object
    if msg.isEmpty then
        ujson.Obj("success" -> false, "err" -> "The message is empty")
    else if session.getCurrentUser.isEmpty then
        ujson.Obj("success" -> false, "err" -> "No user is logged in bro")
    else
        val user = session.getCurrentUser.get
        val mention = if msg.charAt(0) == '@' then Some(msg.substring(1, msg.indexOf(' '))) else None
        var msgReformat = msg

        if mention != None then
          msgReformat = msg.substring(msg.indexOf(" "))
        end if

        if isBotMessage(msg) then
          try {
            val tokenized = tokenizerSvc.tokenize(msg.substring(msg.indexOf(" ")))
            val expr = new Parser(tokenized).parsePhrases()

            // Check if expr is a Command
            expr match {
              case Command(subExpr) => {
                val products = getProductsFromExpr(subExpr)
                var msg = "Votre commande est en cours de préparation. Elle contient : "

                var productsString = ""

                for (product <- products) {
                  if product.brand == None then
                    productsString += product.quantity + " " + product.name + " "
                  else
                    productsString += product.quantity + " " + product.name + " " + product.brand + " "
                }

                msgSvc.add("bot", Layouts.messageContent(msg + productsString, None), session.getCurrentUser, Some(expr))
                val messages = msgSvc.getLatestMessages(20)
                val latests = messagesToString(messages)
                webSockets.foreach(_.send(cask.Ws.Text(latests)))

                val futurList = products.map(product => {
                  synchCommands(session, product)
                })

                // When all the futurs are done, we send a message to the user showing the result
                Future.sequence(futurList).onComplete(ls => {

                  var msg = ""

                  // Check if one of the commands failed
                  if ls.get.exists(_._3 == false) then
                    msg = "Votre commande a partiellement réussie. Elle contient : "
                  else
                    msg = "Votre commande est prête. Elle contient : "

                  var productsString = ""

                  for (product <- ls.get) {
                    productsString += product._1 + " " + product._2 + " "
                  }

                  msgSvc.add("bot", Layouts.messageContent(msg + productsString, None), session.getCurrentUser, Some(expr))
                  val messages = msgSvc.getLatestMessages(20)
                  val latests = messagesToString(messages)
                  webSockets.foreach(_.send(cask.Ws.Text(latests)))
                })

              }
              case _ => {
                val id = msgSvc.add(user, Layouts.messageContent(msgReformat, mention), Some(user), Some(expr))
                val botMessage = analyzerSvc.reply(session)(expr)
                msgSvc.add("bot", Layouts.messageContent(botMessage, None), session.getCurrentUser, None, Some(id))
                val messages = msgSvc.getLatestMessages(20)
                val latests = messagesToString(messages)
                webSockets.foreach(_.send(cask.Ws.Text(latests)))
              }
            }
          }
          catch {
            case e: UnexpectedTokenException => return ujson.Obj("success" -> false, "err" -> e.getMessage)
          }
        else
          msgSvc.add(user, Layouts.messageContent(msgReformat, mention), mention, None)
        end if

        // Add message to the message service
        ujson.Obj("success" -> true, "err" -> "")
    end if
  end send

  def isBotMessage(msg: String) = msg.startsWith("@bot ")

  private def messagesToString(messages : Seq[(String, Frag)]) = {
    messages match {
      case Nil => "No message yet"
      case _ =>
        messages
          .map((user, msg) => Layouts.message(user, msg).toString)
          .mkString
    }
  }

  // TODO - Part 3 Step 4c: Process and store the new websocket connection made to `/subscribe`
  //
  @cask.websocket("/subscribe")
  def subscribe() : cask.WebsocketResult =
      cask.WsHandler { channel =>

        // Save channel and send the latest messages
        webSockets += channel
        val messages = msgSvc.getLatestMessages(20)
        val latests = messagesToString(messages)
        webSockets.foreach(_.send(cask.Ws.Text(latests)))
        //messages.foreach(message => channel.send(cask.Ws.Text(message._2.toString())))

        // Remove channel on disconnect.
        cask.WsActor {
            case cask.Ws.Close(code, reason) =>  webSockets -= channel
        }
      }
  end subscribe

  // TODO - Part 3 Step 4d: Delete the message history when a GET is made to `/clearHistory`
  //
  @getSession(sessionSvc)
  @cask.get("/clearHistory")
  def clearHistory()(session: Session) =
    if !session.getCurrentUser.isEmpty then msgSvc.deleteHistory()
    end if
    cask.Redirect("/")
  end clearHistory
  // TODO - Part 3 Step 5: Modify the code of step 4b to process the messages sent to the bot (message
  //      starts with `@bot `). This message and its reply from the bot will be added to the message
  //      store together.
  //
  //      The exceptions raised by the `Parser` will be treated as an error (same as in step 4b)

  initialize()
end MessagesRoutes
