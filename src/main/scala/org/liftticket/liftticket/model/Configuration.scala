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

import _root_.scala.xml.Text

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.sitemap.{Loc,Menu,NullLocParams}
import _root_.net.liftweb.common.{Empty}
import _root_.net.liftweb.http.S

object Configuration {
  def sections = BaseConfig :: TicketConfig :: EmailConfig :: Nil
}

/**
 * This class maps configuration properties to the database.
 */
class ConfigurationProperty extends LongKeyedMapper[ConfigurationProperty] with IdPK {
  def getSingleton = ConfigurationProperty
  
  object property extends MappedString(this,500)
  object value extends MappedString(this,500)
}
object ConfigurationProperty extends ConfigurationProperty with LongKeyedMetaMapper[ConfigurationProperty]

/** 
 * This class represents a given configuration section. It carries some extra
 * code to provide SiteMap and template functionality in common code.
 */
sealed abstract class ConfigSection(val sectionName : String) extends Enumeration {
  class FieldVal(name : String, val fieldType : FieldType.Value) extends Val(name)
  
  def Value(name : String, fieldType : FieldType.Value) = new FieldVal(name, fieldType)
  
  // Each config section will get its own Loc to simplify menu and page generation
  val menu = Menu(new Loc[NullLocParams] {
	import Loc._
    def name = "config." + sectionName
    def text = S.?(propertyBase + ".title")
    def link = List[String]("config", sectionName)
	def defaultParams = Empty
	def params = Loc.If(() => User.hasPermission(Permissions.EditConfiguration), 
                        "org.liftticket.msg.nopermission") :: Nil
    /*
    def template = 
    def snippets = {
      case "view" =>
    }
    */
  })
  
  val propertyBase = "org.liftticket.config.section." + sectionName
}

object BaseConfig extends ConfigSection("basic") {
  /* This is essentially a "master" password for initial setup and emergency access.
   * You can set it by setting a system property of the same name.
   */
  val ConfigPassword = Value("org.liftticket.config.password", FieldType.Password)
}

object TicketConfig extends ConfigSection("ticket") {
  val AttachmentPath = Value("org.liftticket.config.attachmentroot")
}
 
object EmailConfig extends ConfigSection("email") {
  val EmailOutboundServer = Value("org.liftticket.config.emailoutserver")
  val EmailOutboundPort = Value("org.liftticket.config.emailoutport")
  val EmailOutboundUser = Value("org.liftticket.config.emailoutuser")
  val EmailOutboundPass = Value("org.liftticket.config.emailoutpass")
  val EmailOutboundFrom = Value("org.liftticket.config.emailoutfrom")
}
