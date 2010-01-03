/* 
 * Copyright © 2009, Derek Chen-Becker
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package org.liftticket.liftticket.model

import scala.xml.{NodeSeq,Text}

import net.liftweb.mapper._
import net.liftweb.sitemap.{Loc,Menu}
import net.liftweb.common.{Empty}
import net.liftweb.http.{LiftRules,S,SessionVar,SHtml,js}
import js.JsCmds
import net.liftweb.common.{Box,Empty,Failure,Full}
import net.liftweb.util.Helpers._

/**
 * This object contains global configuration methods and definitions. It also deals
 * with master password handling.
 */
object Configuration {
  /* This is the per-VM master password. It doesn't get stored anywhere and is
     only valid if set from the command line */
  var masterPassword : Box[String] = None
  
  /* This indicates that the user has logged in with a master password and can
     access all of the configuration sections as well as user administration */
  object loggedInWithMasterPass extends SessionVar(false)
  
  // A simple method to log in against the master password
  def authenticateMasterPassword (password : String) : Boolean = 
    masterPassword map { mp =>
      if (mp == password) {
        loggedInWithMasterPass(true)
        true
      } else {
        false
      }
    } openOr false
  
  // Define the various configuration sections
  def sections = TicketConfig :: EmailConfig :: Nil
  
  /* Here we define the master menu for the configuration sections. The various 
     subsections will show up as child menus */
  def menus : List[Menu] = {
	import Loc._

    Menu(Loc("config-master", List("config", "index"), S.?("org.liftticket.config.title"), configMenuIf), sections.map {_.menu} : _*) ::
    Menu(Loc("initialConfig", List("config", "initial"), S.?("org.liftticket.config.initialtitle"), Hidden)) :: Nil
  }
  
  // Define a SiteMap helper to check on permissions
  val configMenuIf = {
    import Loc._
    Loc.If(() => User.hasPermission(Permissions.EditConfiguration) || loggedInWithMasterPass.is, S.?("org.liftticket.msgs.nopermission"))
  }
}

/**
 * This class maps configuration properties to the database.
 */
class ConfigurationProperty extends LongKeyedMapper[ConfigurationProperty] with IdPK {
  def getSingleton = ConfigurationProperty
  
  // The key of the configuration property. Generally, this is taken from the enum name
  object property extends MappedString(this,500)
  
  // The String value of the property
  object value extends MappedString(this,500)
}

object ConfigurationProperty extends ConfigurationProperty with LongKeyedMetaMapper[ConfigurationProperty] {
  // We set up a unique index on the property key so that we can't get duplicates
  override def dbIndexes = UniqueIndex(property) :: Nil
  
  // A simple utility method to either retrieve the current key or set up a new instance
  def getOrNew (name : String) = findAll(By(property, name)) match {
    case Nil => ConfigurationProperty.create.property(name).value("")
    case prop :: Nil => prop
    case _ => throw new Exception("More than one instance found for " + name)
  }
  
  // A simple utility method to retrieve the current key as a Box
  def get (name : String) : Box[ConfigurationProperty] = findAll(By(property, name)) match {
    case Nil => Empty
    case prop :: Nil => Full(prop)
    case _ => Failure("More than one instance found for " + name)
  }
}

/** 
 * This class represents a given configuration section. It carries some extra
 * code to provide SiteMap and template functionality in common code.
 */
sealed abstract class ConfigSection(val sectionName : String) extends Enumeration {
  /**
   * This represents a special enumeration value that carries a validator and an error message if
   * validation fails.
   */
  class FieldVal(name : String, val validator : String => Boolean, val failedValidationKey : String) extends Val(name)
  
  // We define another overload of the Value method to keep consistent with normal Enumeration usage
  def Value(name : String, validator : String => Boolean, msgKey : String) = new FieldVal(name, validator, msgKey)
  
  // Each config section will get its own Menu to simplify menu and page generation
  val menu = {
    import Loc._
    Menu(Loc("config." + sectionName, List[String]("config", sectionName), S.?(propertyBase + ".title"),
         // Instead of using a programmatic template here, we can leverage LiftRules to pull a common template from the WAR
         Template(() => LiftRules.loadResourceAsXml("/templates-hidden/config-template.html") openOr Text("Not found!")),
         Snippet("view", { xhtml : NodeSeq => 
           bind("config", xhtml, "content" -> fieldContent(chooseTemplate("config", "content", xhtml))) 
         }),
    	 Configuration.configMenuIf))
    }
  
  /* We're defining a common field markup generator here to unclutter the menu val. */
  private def fieldContent(template : NodeSeq) : NodeSeq = elements.toList.flatMap { field =>
    val instance = ConfigurationProperty.getOrNew(field.toString)
    
    bind("field", template, 
         "label" -> S.?(field.toString),
    	 "field" -> SHtml.ajaxEditable(instance.value.asHtml, 
                                       instance.value.toForm.map(JsCmds.FocusOnLoad(_)) openOr instance.value.asHtml,
                                       () => field match {
                                         case f : FieldVal => {
                                           if (f.validator(instance.value.is)) {
                                             instance.save
                                           } else {
                                             instance.value(instance.value.was) // reset the value
                                             S.error(S.?(f.failedValidationKey))
                                           }
                                         }
                                         case _ => instance.save
                                       }))
  }
  
  // Generate a default base key name for message lookup, etc.
  val propertyBase = "org.liftticket.config.section." + sectionName
}

object BaseConfig {
  val ConfigPassword = "org.liftticket.config.masterpass"
}

object TicketConfig extends ConfigSection("ticketconfig") {
  val AttachmentPath = Value("org.liftticket.config.attachmentroot", { path : String =>
    val file = new _root_.java.io.File(path)
    file.canWrite && file.isDirectory 
  }, "org.liftticket.config.attachmentroot.invalid")
}
 
object EmailConfig extends ConfigSection("emailconfig") {
  val EmailOutboundServer = Value("org.liftticket.config.emailoutserver")
  val EmailOutboundPort = Value("org.liftticket.config.emailoutport")
  val EmailOutboundUser = Value("org.liftticket.config.emailoutuser")
  val EmailOutboundPass = Value("org.liftticket.config.emailoutpass")
  val EmailOutboundFrom = Value("org.liftticket.config.emailoutfrom")
}
