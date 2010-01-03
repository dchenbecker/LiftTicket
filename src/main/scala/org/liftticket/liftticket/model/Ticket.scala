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

import scala.xml.Text

import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.http.{SessionVar,SHtml}
import net.liftweb.textile.TextileParser

import liftticket.util.{TicketAuditor,TicketUpdate}

/**
 * This is the main class for ticket functionality.
 */
class Ticket extends LongKeyedMapper[Ticket] with IdPK with OneToMany[Long,Ticket] with ManyToMany {
  def getSingleton = Ticket
  
  /** A brief description of the issue. */
  object summary extends MappedString(this, 200)
  
  /** A reference to the user that created the issue. */
  object creator extends MappedLongForeignKey(this,User) {
    override def displayName = "Created by"
  }
  
  /** A detailed description of the issue, in Wiki-ish format */
  object description extends MappedTextarea(this,4000) {
    override def textareaCols = 80
    override def textareaRows = 6
  }
  
  /** Which module does this belong to? */
  object module extends MappedLongForeignKey(this,Module) {
    override def displayName = "Module"
    
    override def asHtml = Text(obj.map(_.name.is) openOr "None") 

    override def toForm = {
      val options = Module.findAll.map(opt => (opt.id.toString, opt.name.is))
      Full(SHtml.select(options, can.map(_.toString), {in : String => apply(in.toLong)}))
    }
  }
  
  /** The release (and implicitly, the module) where this issue was found */
  object releaseFound extends MappedLongForeignKey(this,ModuleRelease) {
    override def displayName = "Found in"
    override def asHtml = Text(obj.map(_.version.is) openOr "None") 

    override def toForm = {
      val options = ModuleRelease.findAll(By(ModuleRelease.module,module.obj)).map(opt => (opt.id.toString, opt.version.is))
      Full(SHtml.select(options, can.map(_.toString), {in : String => apply(in.toLong)}))
    }
  }
  
  /** The release where this issue is or will be fixed */
  object releaseFixed extends MappedLongForeignKey(this,ModuleRelease) {
    override def displayName = "Fixed in"
    override def asHtml = Text(obj.map(_.version.is) openOr "None") 

    override def toForm = {
      val options = ModuleRelease.findAll(By(ModuleRelease.module,module.obj)).map(opt => (opt.id.toString, opt.version.is))
      Full(SHtml.select(options, can.map(_.toString), {in : String => apply(in.toLong)}))
    }
  }
  
  /** The current status of the ticket. */
  object status extends MappedLongForeignKey(this,TicketStatus) {
    override def displayName = "Status"
    override def asHtml = Text(obj.map(_.name.is) openOr "NULL") 

    override def toForm = {
      val options = TicketStatus.findAll.map(opt => (opt.id.toString, opt.name.is))
      Full(SHtml.select(options, can.map(_.toString), {in : String => apply(in.toLong)}))
    }
  }
 
  /** How badly this ticket needs handled. */
  object severity extends MappedLongForeignKey(this,TicketSeverity) {
    override def displayName = "Severity"
    override def asHtml = Text(obj.map(_.name.is) openOr "NULL")
    
    override def toForm = {
      val options = TicketSeverity.findAll.map(opt => (opt.id.toString, opt.name.is))
      Full(SHtml.select(options, can.map(_.toString), {in : String => apply(in.toLong)}))
    }
  }
  
  /** This is a field orthogonal to severity to allow ordering of ticket handling */
  object priority extends MappedInt(this) {
    override def displayName = "Priority"
    override def defaultValue = 0
  }
  
  object dependencies extends MappedOneToMany(TicketDependency, TicketDependency.ticket)

  /** These are the users responsible for the ticket. */
  object owners extends MappedManyToMany(TicketOwner, TicketOwner.ticket, TicketOwner.user, User)
  
  /** These are users that want notifications when the ticket is modified */
  object followers extends MappedManyToMany(TicketFollower, TicketFollower.ticket, TicketFollower.user, User)

  /** This is the person responsible for QA */
  object tester extends  MappedLongForeignKey(this,User)
  
  /** This is the collection of all comments on the ticket */
  object comments extends MappedOneToMany(TicketComment, TicketComment.ticket) {
    def asHtml = {
      <table>
      { all.map(comment => <tr><td>{"%s : %s : %s".format(comment.timestamp, comment.user.asHtml, comment.comment)}</td></tr>)}
      </table>
    }
  }
}

object Ticket extends Ticket with LongKeyedMetaMapper[Ticket] with CRUDOps[Long,Ticket] {
  val instanceName = "Ticket"
  
  override def calcCheckPermissions = {
    import CrudOperations._
    
    {
      case View | ViewAll => CRUDPermissions(true) // TODO: Should we have control on guest users?
      case Create => CRUDPermissions(User.loggedIn_?)
      case _ => CRUDPermissions(false)
    }
  }
  
  // Hold a list of status IDs
  object ShowStatus extends SessionVar[List[Long]](Nil)
  
  override def getListInstances = 
    ShowStatus.is match {
      case Nil => findAll
      case statuses => findAll(ByList(status,statuses)) 
    }
  
  override def listTemplate = {
    <lift:Ticket.filters form="POST" /> ++ {super.listTemplate}
  }
  
  override def viewTemplate = {
    super.viewTemplate ++ {
      currentObject.is.map { ticket => 
      <h2>Comments</h2>
      <table id="comments">
        <tr><th>Time</th><th>User</th></tr>
        <tr><th>Comment</th></tr>
        { 
          ticket.comments.all.flatMap { comment =>
            <tr><td>{comment.timestamp.toString}</td><td>{comment.user.toString}</td></tr>
            <tr>{TextileParser.parse(comment.comment,None).map(_.toHtml).getOrElse(Text("ERROR"))}</tr>
          }
        }
      </table>
      } openOr Text("")
    }
        
  }
  
  override val calcFields = {
    Field(Text("Ticket Id"), instance => instance.id.asHtml) ::
    defaultField(summary, true, true, true) ::
    defaultField(module, true, true, true) ::
    defaultField(creator,false,true,false) ::
    ajaxEditableField(releaseFound, Permissions.EditTicketMetadata, false, true, true) ::
    ajaxEditableField(releaseFixed, Permissions.EditTicketMetadata, false, true, true) ::
    ajaxEditableField(status, Permissions.EditTicketMetadata, false, true, true,
                      Full(instance => "Status changed to %s".format(instance.status.obj.map(_.name.is) openOr "Unknown"))) ::
    ajaxEditableField(severity, Permissions.EditTicketMetadata, false, true, true,
                      Full(instance => "Severity changed to %s".format(instance.severity.obj.map(_.name.is) openOr "Unknown"))) ::
    ajaxEditableField(priority, Permissions.EditTicketMetadata, false, true, true) ::
    defaultField(description, true, true, false) ::
    Field(Text("Comments"), false, true, false, Field.noop, instance => instance.comments.asHtml, Field.noop) :: Nil
  }
  
  private def ajaxEditableField(field : MappedField[_,MapperType], permission : Permissions.Value,
                                edit : Boolean, view : Boolean, list : Boolean) : Field =
    ajaxEditableField(field,permission,edit,view,list,Empty)
  
  private def ajaxEditableField(field : MappedField[_,MapperType], permission : Permissions.Value,
                                edit : Boolean, view : Boolean, list : Boolean, auditMessage : Box[MapperType => String]) : Field = {
    val editFunc = { 
      instance : MapperType => (
        instance.fieldByName(field.name).map{mf : MappedField[_,MapperType] =>
          if (User.hasPermission(permission)) {
            SHtml.ajaxEditable(mf.asHtml,
                               mf.toForm.openOr(Text("")),
                               () => {
                                 val logMessage = auditMessage.map(_(instance)) openOr "%s changed to \"%s\"".format(field.name, mf.is) 
                                 instance.comments += TicketComment.create.comment(logMessage)
                                 instance.save
                                 TicketAuditor.processTicketChange(TicketUpdate(instance, logMessage, Full(mf)))
                               }
            )
          } else {
            mf.asHtml
          }	          
        }
      ) openOr Text("Could not generate form for " + field.name)                 
    }
    
    val displayFunc = { instance : MapperType => 
      instance.fieldByName(field.name).map{mf : MappedField[_,MapperType] => mf.asHtml} openOr Text("No display")               
    }
    
    Field(Text(field.displayName),
          edit, view, list,
          displayFunc,
          editFunc,
          displayFunc // Don't do AJAX in lists
    )
  }
  
  import net.liftweb.sitemap._
  import Loc._
  import net.liftweb.http.S
  def configMenus = 
    List(Menu(Loc("TicketConfig", List("config", "ticket"), "Ticket Info Configuration",
                  If(() => TicketMetaDataRules.canViewMetaData, S.?("You do not have permission to view this page"))),
              (TicketStatus.menus ::: TicketSeverity.menus) : _*
    ))
  
  
  // OVerride this so that we can ensure default values
  override def beforeCreate = onCreate _ :: super.beforeCreate
  override def afterCreate = { ticket : Ticket =>
    // Send a create message
    TicketAuditor.processTicketChange(TicketUpdate(ticket, "Ticket created", Empty))
  } :: super.afterCreate
   
  def onCreate (ticket : Ticket) = {
    ticket.status(TicketStatus.default) 
    ticket.severity(TicketSeverity.default)
    ticket.creator(User.currentUser)
    ticket.comments += TicketComment.create.comment("Ticket created")
  }
}

object TicketMetaDataRules {
  def canViewMetaData = 
    User.hasPermission(Permissions.EditTicketMetaData) || 
    Configuration.loggedInWithMasterPass.is
  
  val configCheckPerms : PartialFunction[CrudOperations.Value, CRUDPermissions]  = {
    import CrudOperations._ 
    {
      case Create | Edit | Delete => 
        CRUDPermissions(canViewMetaData)
      case _ => CRUDPermissions(true)
    }
  }
}

/**
 * This class defines the possible ticket status codes.
 */
class TicketStatus extends LongKeyedMapper[TicketStatus] with IdPK {
  def getSingleton = TicketStatus
  /** A descriptive name for this status */
  object name extends MappedString(this,200)
  
  object means_closed extends MappedBoolean(this) {
    override def displayName = "Indicates closed"
  } 
  
  object is_default extends MappedBoolean(this) {
    override def defaultValue = false
  }
}
object TicketStatus extends TicketStatus with LongKeyedMetaMapper[TicketStatus] with CRUDOps[Long,TicketStatus] {
  val instanceName = "Status"
  override val pluralName = "Statuses"
  
  override def calcCheckPermissions = TicketMetaDataRules.configCheckPerms
  
  override def save(ts : TicketStatus) = {
    // Ensure that only one status is set to the default by unsetting the current
    if (ts.is_default.is) {
      findAll(By(is_default, true)).foreach(_.is_default(false).save)
    }
    super.save(ts)
  }
  
  def default =
    findAll(By(is_default, true)) match {
      case s :: xs => s.id.is
      case _ => throw new Exception("Default Status is not defined")
    }
}

/**
 * This class defines the possible ticket severities.
 */
class TicketSeverity extends LongKeyedMapper[TicketSeverity] with IdPK {
  def getSingleton = TicketSeverity
  /** A descriptive name for this severity */
  object name extends MappedString(this,200)
  
  object is_default extends MappedBoolean(this) {
    override def defaultValue = false
  }
}
object TicketSeverity extends TicketSeverity with LongKeyedMetaMapper[TicketSeverity] with CRUDOps[Long,TicketSeverity] {
  val instanceName = "Severity"
  override val pluralName = "Severities"
   
  override def calcCheckPermissions = TicketMetaDataRules.configCheckPerms
  
  override def save(ts : TicketSeverity) = {
    // Ensure that only one severity is set to the default by unsetting the current
    if (ts.is_default.is) {
      findAll(By(is_default, true)).foreach(_.is_default(false).save)
    }
    super.save(ts)
  }

  def default =
    findAll(By(is_default, true)) match {
      case s :: xs => s.id.is
      case _ => throw new Exception("Default Severity is not defined")
    }
}

object TicketDependencyType extends Enumeration {
  val InternalDependency = Value(1,"Internal Dependency")
  val InternalDuplicate = Value(2, "Duplicate")
  val ExternalDependency = Value(3, "External Dependency")
}
class TicketDependency extends LongKeyedMapper[TicketDependency] with IdPK {
  def getSingleton = TicketDependency
    
  /** Which ticket are we associated with? */
  object ticket extends MappedLongForeignKey(this,Ticket)
  
  /** This classifies what kind of dependency this is */
  object depType extends MappedEnum(this, TicketDependencyType)
  
  /** If this is an internal dependency or duplicate, we record the other ticket */
  object internal extends MappedLongForeignKey(this,Ticket)
  
  /** If this is an external dependency then we record the URL to the external ticket */
  object external extends MappedString(this,1000)
}
object TicketDependency extends TicketDependency with LongKeyedMetaMapper[TicketDependency]

/**
 * This class represents the many-to-many link between a ticket and an owner,
 * since tickets can be owned by more than one user.
 */
class TicketOwner extends LongKeyedMapper[TicketOwner] with IdPK {
  def getSingleton = TicketOwner
  object ticket extends MappedLongForeignKey(this,Ticket)
  object user extends MappedLongForeignKey(this,User)
}
object TicketOwner extends TicketOwner with LongKeyedMetaMapper[TicketOwner]

/**
 * This class represents people who want updated when ticket info changes.
 */
class TicketFollower extends LongKeyedMapper[TicketFollower] with IdPK {
  def getSingleton = TicketFollower
  object ticket extends MappedLongForeignKey(this,Ticket)
  object user extends MappedLongForeignKey(this,User)
}
object TicketFollower extends TicketFollower with LongKeyedMetaMapper[TicketFollower]

/**
 * This class represents a ticket comment with optional attachments.
 */
class TicketComment extends LongKeyedMapper[TicketComment] with IdPK with OneToMany[Long,TicketComment] {
  def getSingleton = TicketComment
  object ticket extends MappedLongForeignKey(this,Ticket)
  object timestamp extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date
  }  
  object user extends MappedLongForeignKey(this,User) {
    override def defaultValue = User.currentUser.map(_.id.is) openOr 0
    override def asHtml = {
      Text(obj.map(_.email.is) openOr "Unknown")
    }
  }
  object comment extends MappedString(this,4000)
  /** Each comment can have 0 or more file attachments with it */
  object attachments extends MappedOneToMany(TicketCommentAttachment, TicketCommentAttachment.comment)
}
object TicketComment extends TicketComment with LongKeyedMetaMapper[TicketComment]

/**
 * This class represents an attachment for a comment.
 */
class TicketCommentAttachment extends LongKeyedMapper[TicketCommentAttachment] with IdPK {
  def getSingleton = TicketCommentAttachment
  object comment extends MappedLongForeignKey(this,TicketComment)
  /** Where the attachment is stored locally in the LiftTicket system */
  object path extends MappedString(this,500)
  /** A brief description of the attachment */
  object description extends MappedString(this,500)
}
object TicketCommentAttachment extends TicketCommentAttachment with LongKeyedMetaMapper[TicketCommentAttachment]
