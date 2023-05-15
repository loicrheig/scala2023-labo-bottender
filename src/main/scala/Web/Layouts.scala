package Web

import scalatags.Text.all._
import scalatags.Text.tags2
import scalatags.Text.TypedTag

/** Assembles the method used to layout ScalaTags
  */
object Layouts:
// You can use it to store your methods to generate ScalaTags.
  val generate = () => {
    html(
      body(
        h1(backgroundColor:="blue", color:="red")("This is my title"),
        div(backgroundColor:="blue", color:="red")(
          p(cls := "content")(
            "This is my first paragraph"
          ),
          a(opacity:=0.9)(
            p(cls := "contentpara")("Goooogle")
          )
        )
      )
    )
  }

  val heading = () => {
    head(
      link(
        rel := "stylesheet",
        href := "/css/main.css"
      ),
      script(src := "/js/main.js")
    )
  }

  val header = () => {
    tag("nav")(
      a(href := "/")(
        cls := "nav-brand",
        p("Bot-tender")
      ),
      
      div(
        cls := "nav-item",
        a(href := "/login")(
          p("Log in")
        )
      ),
    )
  }

  val messageList = () => {
    div(cls := "content")(
      div(id := "boardMessage")(
        div(cls := "msg")(
          css("text-align") := "center",
          css("vertical-align") := "middle",
          css("padding") := "40px",
          span(
            cls := "author"
          ),
          span(cls := "msg-content")(
            span(
              cls := "mention"
            ),
            "Please wait, the messages are loading."
          )
        )
      ),
      form(
        id := "msgForm",
        onsubmit := "submitMessageForm(); return false;",
        div(
          id := "errorDiv",
          cls := "errorMsg"
        ),
        label(
          `for` := "messageInput",
          "Your message:"
        ),
        input(
          id := "messageInput",
          `type` := "text",
          placeholder := "Type your message here"
        ),
        input(
          `type` := "submit",
        )
      )
    )
  }

  val basicPage = (content : TypedTag[String]) => {
    html(
      heading(),
      body(
        header(),
        content,
      )
    )
  }

  val basicForm = (leftText : String, placeHolder : String, textId : String, actionS : String, methodS : String, title : String) => {
    div(
      h1(title),
      form(
        action := actionS,
        method := methodS,
        label(
          leftText
        ),
        input(
          id := textId,
          `type` := "text",
          name:=textId,
          placeholder := placeHolder
        ),
        input(
          `type` := "submit",
          "Envoyer"
        )
      )
    )
  }

  val loginForm = (errormsg : Option[String]) => {
    div(
      div(cls := "errorMsg")(errormsg.getOrElse("")),
      basicForm("Username: ", "Type your username here", "usernameInput", "/login", "post", "login"),
      basicForm("Username: ", "Type your username here", "usernameInput", "/register", "post", "register")
    )
  }

end Layouts
