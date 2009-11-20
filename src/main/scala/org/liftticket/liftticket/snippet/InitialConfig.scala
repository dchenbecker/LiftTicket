package org.liftticket.liftticket.snippet

import _root_.scala.xml.NodeSeq

import _root_.net.liftweb.http.{S,SHtml}
import _root_.net.liftweb.util.Helpers._

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
    
    bind("login", xhtml, "pass" -> SHtml.password("", doLogin))
  }
}
