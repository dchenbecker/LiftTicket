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
package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{By, DB, DBLogEntry, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import _root_.java.sql.{Connection, DriverManager}
import _root_.org.liftticket.liftticket.model._
import _root_.org.liftticket.liftticket.snippet.UserAdmin
import _root_.net.liftweb.http.provider.HTTPRequest

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // Tell Lift where to find packages
    LiftRules.addToPackages("org.liftticket.liftticket")
    
    // Set up some custom resource bundles for message and field labels (under src/main/resources)
    LiftRules.resourceNames = "Messages" :: "FieldLabels" :: Nil
    
    // Tie Mapper to the database
    DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)

    // We'll do some basic logging of DB activity
    DB.addLogFunc { 
      case (query, time) => {
        Log.info("All queries took " + time + "ms: ")
        query.allEntries.foreach({ case DBLogEntry(stmt, duration) => Log.info(stmt + " took " + duration + "ms")})
        Log.info("End queries")
      }
    }
  
    // Construct our model as needed
    Schemifier.schemify(true, Log.infoF _,
                        ConfigurationProperty,
                        Module,
                        ModuleRelease,
                        User,
                        UserPermission,
                        UserRole,
                        Role,
                        RolePermission,
                        Ticket,
                        TicketStatus,
                        TicketSeverity,
                        TicketDependency,
                        TicketOwner,
                        TicketFollower,
                        TicketComment,
                        TicketCommentAttachment
    )
  
    /* If a config password is specified as a system property, we set it */
    System.getProperty(BaseConfig.ConfigPassword) match {
      case null =>
      case newPass => 
        Configuration.masterPassword = Full(newPass)
        Log.info("The master password has been set. Please restart the app as soon as you're done configuring it.")
    }
    
    checkDefaultMetadata()
  
    // Finally, set up our SiteMap
    LiftRules.setSiteMap(SiteMap(buildMenus : _*))
  }
  
  // A utility method to construct our SiteMap
  def buildMenus : List[Menu] = 
    Menu(Loc("home", List("index"), S.?("Home"))) ::
    Ticket.menus :::
    User.sitemap :::
    UserAdmin.menus :::
    Module.menus :::
    ModuleRelease.menus :::
    Ticket.configMenus :::
    Configuration.menus
  
  /**
   * This method makes sure that we have some ticket metadata (status, severity, etc)
   * in the database. If nothing exists it will populate with default values.
   */
  def checkDefaultMetadata() {
    if (TicketStatus.findAll.isEmpty) {
      Log.info("Adding default ticket status codes")
      TicketStatus.create.name("New").is_default(true).save
      TicketStatus.create.name("Incomplete").is_default(false).save
      TicketStatus.create.name("Invalid").is_default(false).save
      TicketStatus.create.name("Won't Fix").is_default(false).save
      TicketStatus.create.name("Confirmed").is_default(false).save
      TicketStatus.create.name("Triaged").is_default(false).save
      TicketStatus.create.name("Confirmed").is_default(false).save
      TicketStatus.create.name("Fix Comitted").is_default(false).save
      TicketStatus.create.name("Fix Release").is_default(false).save
    }
    
    if (TicketSeverity.findAll.isEmpty) {
      Log.info("Adding default ticket severities")
      TicketSeverity.create.name("Low").is_default(false).save
      TicketSeverity.create.name("Medium").is_default(true).save
      TicketSeverity.create.name("High").is_default(false).save
    }
  }
}



/**
* Database connection calculation
*/
object DBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4

  private def createOne: Box[Connection] = try {
    val driverName: String = Props.get("db.driver") openOr
    "org.h2.Driver"

    val dbUrl: String = Props.get("db.url") openOr
    "jdbc:h2:LiftTicketDB"

    Class.forName(driverName)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>
        DriverManager.getConnection(dbUrl, user, pwd)

      case _ => DriverManager.getConnection(dbUrl)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
        case Nil if poolSize < maxPoolSize =>
          val ret = createOne
        poolSize = poolSize + 1
        ret.foreach(c => pool = c :: pool)
        ret

        case Nil => wait(1000L); newConnection(name)
        case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}
