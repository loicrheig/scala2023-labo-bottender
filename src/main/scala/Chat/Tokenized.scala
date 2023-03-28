package Chat

import Chat.Token.*
import Utils.SpellCheckerService

trait Tokenized:
  /**
    * Get the next token of the user input, or EOL if there is no more token.
    * @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token)

class TokenizedImpl(var tokens: Array[(String, Token)]) extends Tokenized:
  var counter = 0
  // TODO - Part 1 Step 3
  def nextToken(): (String, Token) = {
    //with counter
    if (counter < tokens.length) {
      val token = tokens(counter)
      counter += 1
      println("counter: " + counter) 
      token
    } else {
      (EOL.toString, EOL)
    }
  }

end TokenizedImpl
