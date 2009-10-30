package org.liftticket.liftticket.snippet

import liftticket.model._

class TestSnippet {
  def render = {
    val mod = Module.create.name("Test").description("Just a test")
    mod.save
    mod.asHtml
  }
}
