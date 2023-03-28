import org.scalatest.*
import org.scalatest.matchers.should
import org.scalatest.propspec.AnyPropSpec
import prop.*

import java.io.ByteArrayOutputStream
import Utils.{Dictionary, SpellCheckerService, SpellCheckerImpl}
import Chat.TokenizerService
import Chat.Token

class BotTenderTokenizerInputSuite extends AnyPropSpec with TableDrivenPropertyChecks with should.Matchers {
    val spellCheckerSvc: SpellCheckerService = new SpellCheckerImpl(Dictionary.dictionary)
    val tokenizerSvc: TokenizerService = new TokenizerService(spellCheckerSvc)
    
    val evaluateInput = MainTokenizer.evaluateInput(tokenizerSvc)

    property("inputting asd input") {
        //evaluateInput("Bonhjoue")
        var result = tokenizerSvc.tokenize("Bonjour 5")

        // Check that after the tokenization, all the tokens are correct
        result.nextToken() should equal(("bonjour", Token.BONJOUR))
        result.nextToken() should equal(("5", Token.NUM))

    }

    // You can use this test to debug any input
    property("inputting correct input") {
        //evaluateInput("Bonhjoue")
        var result = tokenizerSvc.tokenize("Bonjour je veux 5 bières svp.")

        // Check that after the tokenization, all the tokens are correct
        result.nextToken() should equal(("bonjour", Token.BONJOUR))
        result.nextToken() should equal(("je", Token.JE))
        result.nextToken() should equal(("vouloir", Token.VOULOIR))
        result.nextToken() should equal(("5", Token.NUM))
        result.nextToken() should equal(("biere", Token.PRODUIT))
        result.nextToken() should equal(("svp", Token.SVP))
        result.nextToken() should equal(("EOL", Token.EOL))

    }

    property("inputting wrong input") {
        //evaluateInput("Bonhjoue")
        var result = tokenizerSvc.tokenize("Hello _Brian jeu vluxi 5 bieeres svp.")

        // Check that after the tokenization, all the tokens are correct
        result.nextToken() should equal(("bonjour", Token.BONJOUR))
        result.nextToken() should equal(("_Brian", Token.PSEUDO))
        result.nextToken() should equal(("je", Token.JE))
        result.nextToken() should equal(("vouloir", Token.VOULOIR))
        result.nextToken() should equal(("5", Token.NUM))
        result.nextToken() should equal(("biere", Token.PRODUIT))
        result.nextToken() should equal(("svp", Token.SVP))
        result.nextToken() should equal(("EOL", Token.EOL))

    }

    property("inputting 'quitter'") {
        // capture output for testing therefore it is not shown in the terminal
        val outCapture = new ByteArrayOutputStream
        Console.withOut(outCapture) {
            evaluateInput("quitter") should equal(false)
        }
        outCapture.toString() should include ("Adieu.")
    }

    property("inputting 'santé !'") {
        evaluateInput("santé !") should equal(true)
    }
}
