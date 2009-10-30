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
package org.liftticket.liftticket.util

import _root_.scala.collection.mutable.HashMap

import _root_.net.liftweb.actor.LiftActor
import _root_.net.liftweb.common.{Box,Empty,Full,SimpleActor}
import _root_.net.liftweb.mapper.MappedField
import _root_.net.liftweb.http.ListenerManager
import _root_.org.liftticket.liftticket.model.{Ticket,TicketAuditEntry}

/**
 * This class encapsulates information about changes to a given ticket.
 */
sealed case class TicketUpdate(ticket : Ticket, description : String, field : Box[MappedField[_,_]])

/**
 * This class acts as a central arbiter for ticket update information.
 */
object TicketAuditor {
  def processTicketChange(update : TicketUpdate) {
    
  }
}

/**
 * This actor is responsible for marshalling the updates to the dashboard for
 * system status.
 */
object DashboardActor extends LiftActor with ListenerManager {
  var msgs = List[TicketUpdate]()
  
  def createUpdate = msgs
  
  override def highPriority = {
    case tu @ TicketUpdate(_,_,_) => msgs = tu :: (msgs take 9) ; updateListeners // TODO: make the number configurable
  }
}

/**
 * This actor is responsible for sending updates on individual tickets.
 */
object Ticketactor extends LiftActor {
  sealed case class AddTicketListener(ticketId : Long, actor : SimpleActor[Any])
  sealed case class RemoveTicketListener(ticketId : Long, actor : SimpleActor[Any])
  
  val ticketListeners = new HashMap[Long,List[SimpleActor[Any]]] {
    override def default(id : Long) = Nil
  }
  
  def messageHandler = {
    case tu @ TicketUpdate(ticket,_,_) => updateTicketListeners(tu)
    case AddTicketListener(id, actor) => ticketListeners.update(id, actor :: ticketListeners(id))
    case RemoveTicketListener(id, actor) => ticketListeners.update(id, ticketListeners(id) filter (_ ne actor))
  }
  
  private def updateTicketListeners(tu : TicketUpdate) {
    ticketListeners(tu.ticket.id.is) foreach (_ ! tu)
  }
}