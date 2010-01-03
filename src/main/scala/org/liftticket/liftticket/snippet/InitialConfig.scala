package org.liftticket.liftticket.snippet

import scala.xml.NodeSeq

import net.liftweb.http.{js,S,SHtml}
import js.JsCmds.FocusOnLoad
import net.liftweb.util.Helpers._

import liftticket.model.Configuration

/**
 * This class defines a simple login page that allows someone to log in with
 * the master config password.
 */
class InitialConfig {
  def render (xhtml : NodeSeq) : NodeSeq = {
    def doLogin(password : String) {
      if (Configuration.authenticateMasterPassword(password)) {
        S.redirectTo("/config/index")
      } else {
        S.error("Invalid password")
      }
    }
    
    bind("login", xhtml, "pass" -> FocusOnLoad(SHtml.password("", doLogin)))
  }
}
