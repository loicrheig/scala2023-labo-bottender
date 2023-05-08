package Web

/** Assembles the routes dealing with static files.
  */
class StaticRoutes()(implicit val log: cask.Logger) extends cask.Routes:
  // TODO - Part 3 Step 1: Make the resources files (`.js` and `.css`) available to the browser.
  //      Do not forget to link to them from your HTML.

  @cask.staticResources("/css/main.css")
  def staticResourceRoutes() = "css/main.css"

  @cask.staticResources("/js/main.js")
  def staticResourceRoutes2() = "js/main.js"

  initialize()
end StaticRoutes
