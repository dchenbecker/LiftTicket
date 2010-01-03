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
package org.liftticket.liftticket.snippet

import scala.xml.NodeSeq

import net.liftweb.common._
import net.liftweb.http.SHtml
import net.liftweb.mapper.By

import liftticket.model.{TicketStatus,Ticket => LTicket}

/**
 * This class handles some ancillary Ticket-related forms and display
 */
class Ticket {
  // TODO: Make this dynamic
  def filters (xhtml : NodeSeq) : NodeSeq = {
    val options = List((Nil, "All"), 
                       (TicketStatus.findAll(By(TicketStatus.means_closed,false)).map(_.id.is), "Open"),
                       (TicketStatus.findAll(By(TicketStatus.means_closed,true)).map(_.id.is), "Closed")
    )
    
    val default = if (LTicket.ShowStatus.is.isEmpty) Empty else Full(LTicket.ShowStatus.is)
    
    <table>
      <tr>
        <th>Status:</th>
        <td>{ SHtml.selectObj(options, default, {in : List[Long] => LTicket.ShowStatus(in)}) }</td>
        <td><input type="submit" value="Search" /></td>
      </tr>
    </table>
  }
}
