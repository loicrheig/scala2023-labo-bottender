package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg){}

class Parser(tokenized: Tokenized):
  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = tokenized.nextToken()

  /** "Eats" the expected token and returns it value, or terminates with an error. */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type Token */
  private def expected(token: Token, more: Token*): Nothing =
    expected(more.prepended(token))
  private def expected(tokens: Seq[Token]): Nothing =
    val expectedTokens = tokens.mkString(" or ")
    throw new UnexpectedTokenException(s"Expected: $expectedTokens, found: $curToken")


  /*private def authHandle() =
    readToken()
    if curToken == ETRE then
      readToken()
      if curToken == ASSOIFFE then
        readToken()
        Thirsty
      else if curToken == AFFAME then
        readToken()
        Hungry
      else expected(ASSOIFFE, AFFAME)
    else expected(ETRE)*/

  private def commandHandle() : ExprTree =
    val quantity : Int = eat(NUM).toInt
    val productType = eat(PRODUIT)
    var productBrand : Option[String] = None
    readToken()
    if curToken == MARQUE then
      productBrand = Some(curValue)
      readToken()
    if curToken == ET then
      val tmp = commandHandle()
      return AndOrder(BasicOrder(Product(quantity, productType, productBrand)), tmp)
    else if curToken == OU then
      val tmp = commandHandle()
      return OrOrder(BasicOrder(Product(quantity, productType, productBrand)), tmp)
    else if curToken == EOL then
      return BasicOrder(Product(quantity, productType, productBrand))
    else expected(ET, OU, EOL)

  /** the root method of the parser: parses an entry phrase */
  // TODO - Part 2 Step 4
  def parsePhrases() : ExprTree =
    if curToken == BONJOUR then readToken()
    if curToken == JE then
      readToken()
      if curToken == ETRE then
        readToken()
        if curToken == ASSOIFFE then
          readToken()
          return Thirsty
        else if curToken == AFFAME then
          readToken()
          return Hungry
        else if curToken == PSEUDO then
          return Auth(curValue)
        else expected(ASSOIFFE, AFFAME, PSEUDO)
      else if curToken == ME then
        eat(APPELER)
        eat(PSEUDO)
        return Auth(curValue)
      else if curToken == VOULOIR then
        readToken()
        if curToken == COMMANDER then
          return commandHandle()
        else if curToken == CONNAITRE then
          eat(MON)
          eat(SOLDE)
          // todo
      else expected(ETRE, ME, VOULOIR)
    else expected(BONJOUR, JE)