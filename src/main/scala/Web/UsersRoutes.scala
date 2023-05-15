package Web

import Data.{AccountService, SessionService, Session}
import Layouts.loginForm, Layouts.basicPage
import cask.model.Request
import Decorators.getSession

/** Assembles the routes dealing with the users:
  *   - One route to display the login form and register form page
  *   - One route to process the login form and display the login success page
  *   - One route to process the register form and display the register success
  *     page
  *   - One route to logout and display the logout success page
  *
  * The username of the current session user is stored inside a cookie called
  * `username`.
  */
class UsersRoutes(accountSvc: AccountService, sessionSvc: SessionService)(
    implicit val log: cask.Logger
) extends cask.Routes:
  // TODO - Part 3 Step 3a: Display a login form and register form page for the following URL: `/login`.
  @cask.get("/login")
  def login() = {
    basicPage(
      loginForm(None),
      // Check if the response has an error message
    )
  }
  // TODO - Part 3 Step 3b: Process the login information sent by the form with POST to `/login`,
  //      set the user in the provided session (if the user exists) and display a successful or
  //      failed login page.
  //

  @getSession(
    sessionSvc
  )
  @cask.postForm("/login")
  def login(usernameInput : String)(session : Session) = {
    // If input is empty display error
      if usernameInput.isEmpty then 
        basicPage(
          loginForm(Some("username empty")),
          // Check if the response has an error message
        )
      // If account exists.
      else if !accountSvc.isAccountExisting(usernameInput) then
          session.setCurrentUser(usernameInput)
          basicPage(
          loginForm(Some("Account does not exist")),
          // Check if the response has an error message
        )
      else if session.getCurrentUser != None then
          basicPage(
          loginForm(Some("Already logged in")),
          // Check if the response has an error message
        )
      // Otherwise
      else 
        basicPage(
          loginForm(Some("Success")),
          // Check if the response has an error message
        )
  }
  // TODO - Part 3 Step 3c: Process the register information sent by the form with POST to `/register`,
  //      create the user, set the user in the provided session and display a successful
  //      register page.
  //
  @getSession(
    sessionSvc
  )
  @cask.postForm("/register")
  def register(usernameInput : String)(session : Session) = {
    // If input is empty display error
      if usernameInput.isEmpty then 
        basicPage(
          loginForm(Some("username empty")),
          // Check if the response has an error message
        )
      // If account exists.
      else if accountSvc.isAccountExisting(usernameInput) then
          basicPage(
          loginForm(Some("Account already existing")),
          // Check if the response has an error message
        )
      else if session.getCurrentUser != None then
          basicPage(
          loginForm(Some("Already logged in")),
          // Check if the response has an error message
        )
      // Otherwise
      else 
        accountSvc.addAccount(usernameInput)
        session.setCurrentUser(usernameInput)
        basicPage(
          loginForm(Some("Success")),
          // Check if the response has an error message
        )
  }
  // TODO - Part 3 Step 3d: Reset the current session and display a successful logout page.
  @getSession(
    sessionSvc
  )
  @cask.get("/logout")
  def logout()(session : Session) = {
    session.reset()
    basicPage(
      loginForm(Some("Success logout")),
    )
  }

  initialize()
end UsersRoutes
