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

import _root_.net.liftweb.mapper._

/**
 * This is the main class for ticket functionality.
 */
class Ticket extends LongKeyedMapper[Ticket] with IdPK with OneToMany[Long,Ticket] with ManyToMany {
  def getSingleton = Ticket
  
  /** A brief description of the issue. */
  object summary extends MappedString(this, 200)
  
  /** A reference to the user that created the issue. */
  object creator extends MappedLongForeignKey(this,User)
  
  /** A detailed description of the issue, in Wiki-ish format */
  object description extends MappedTextarea(this,4000) {
    override def textareaCols = 80
    override def textareaRows = 6
  }
  
  /** The release (and implicitly, the module) where this issue was found */
  object releaseFound extends MappedLongForeignKey(this,ModuleRelease)
  
  /** The release where this issue will be fixed */
  object releaseFixed extends MappedLongForeignKey(this,ModuleRelease)
  
  /** The current status of the ticket. */
  object status extends MappedLongForeignKey(this,TicketStatus)
  
  /** How badly this ticket needs handled. */
  object severity extends MappedLongForeignKey(this,TicketSeverity)
  
  /** This is a field orthogonal to severity to allow ordering of ticket handling */
  object priority extends MappedIntIndex(this)
  
  object dependencies extends MappedOneToMany(TicketDependency, TicketDependency.ticket)

  /** These are the users responsible for the ticket. */
  object owners extends MappedManyToMany(TicketOwner, TicketOwner.ticket, TicketOwner.user, User)
  
  /** These are users that want notifications when the ticket is modified */
  object followers extends MappedManyToMany(TicketFollower, TicketFollower.ticket, TicketFollower.user, User)

  /** This is the person responsible for QA */
  object tester extends  MappedLongForeignKey(this,User)
  
  /** This is the collection of all comments on the ticket */
  object comments extends MappedOneToMany(TicketComment, TicketComment.ticket)

  /** This is a collection of audit log entries that record all actions on the ticket */
  object audits extends MappedOneToMany(TicketAuditEntry, TicketAuditEntry.ticket)
}
object Ticket extends Ticket with LongKeyedMetaMapper[Ticket]

/**
 * This class defines the possible ticket status codes.
 */
class TicketStatus extends LongKeyedMapper[TicketStatus] with IdPK {
  def getSingleton = TicketStatus
  /** A descriptive name for this status */
  object name extends MappedString(this,200)
}
object TicketStatus extends TicketStatus with LongKeyedMetaMapper[TicketStatus]

/**
 * This class defines the possible ticket severities.
 */
class TicketSeverity extends LongKeyedMapper[TicketSeverity] with IdPK {
  def getSingleton = TicketSeverity
  /** A descriptive name for this severity */
  object name extends MappedString(this,200)
}
object TicketSeverity extends TicketSeverity with LongKeyedMetaMapper[TicketSeverity]

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
  object user extends MappedLongForeignKey(this,User)
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

/**
 * This class represents a ticket audit log showing all changes and who made them.
 */
class TicketAuditEntry extends LongKeyedMapper[TicketAuditEntry] with IdPK {
  def getSingleton = TicketAuditEntry
  object ticket extends MappedLongForeignKey(this,Ticket)
  object timestamp extends MappedDateTime(this)
  object user extends MappedLongForeignKey(this,User)
  object log extends MappedString(this,1000)
}
object TicketAuditEntry extends TicketAuditEntry with LongKeyedMetaMapper[TicketAuditEntry]