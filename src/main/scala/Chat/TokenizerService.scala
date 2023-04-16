package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  // TODO - Part 2 step 1
  def tokenize(input: String): Tokenized = {
    val regex = "[^ a-zA-Z0-9_]"
    val trimmed = input.trim.replaceAll(" +", " ").replaceAll("'", " ").replaceAll(regex, "")

    trimmed.split(" ").map(wordToToken) match {
      case Array() => new TokenizedImpl(Array((EOL.toString, EOL)))
      case tokens => new TokenizedImpl(tokens :+ (EOL.toString, EOL))
    }    
  }

  private def wordToToken(word: String): (String, Token) = {
    val normalizedWord = spellCheckerSvc.getClosestWordInDictionary(word)
    (normalizedWord, getToken(normalizedWord))
  }

  private def getToken(normalizedWord : String) : Token ={
    println(normalizedWord)
    normalizedWord match {
      case "bonjour"                              => BONJOUR
      case "je"                                   => JE
      case "etre"                                 => ETRE
      case "vouloir"                              => VOULOIR
      case "assoiffe"                             => ASSOIFFE
      case "affame"                               => AFFAME
      case "biere"                                => PRODUIT
      case "croissant"                            => PRODUIT
      case "et"                                   => ET
      case "ou"                                   => OU
      case "svp"                                  => SVP
      case "boxer"                                => MARQUE
      case "farmer"                               => MARQUE
      case "wittekop"                             => MARQUE
      case "punkipa"                              => MARQUE
      case "jackhammer"                           => MARQUE
      case "tenebreuse"                           => MARQUE
      case "prix"                                 => PRIX
      case "solde"                                => SOLDE
      case "me"                                   => ME
      case "combien"                              => COMBIEN
      case "quel"                                 => QUEL
      case "le"                                   => LE
      case "de"                                   => DE
      case "couter"                               => COUTER
      case "commander"                            => COMMANDER
      case "connaitre"                            => CONNAITRE
      case "mon"                                  => MON
      case "appeler"                              => APPELER
      

      case _ if normalizedWord.startsWith("_")    => PSEUDO
      case _ if normalizedWord.forall(_.isDigit)  => NUM
      case _                                      => UNKNOWN
    }
  }
end TokenizerService
