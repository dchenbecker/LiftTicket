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

/**
 * This enum defines all of the permissions available to users. We're using
 * a programmatic definition to keep the meaning with the bytes ;)
 */
object Permissions extends Enumeration {
  // Normal Ticket permissions
  val AddComment = Value(1, "Add Comment")
  val AddFileAttachment = Value(2, "Add File Attachment")
  val CreateTicket = Value(3, "Create Ticket")
  val EditTicketFollow = Value(4, "Follow/Unfollow Ticket")
  val EditTicketOwner = Value(5, "Edit Ticket Ownership")
  val EditTicketStatus = Value(6, "Edit Ticket Status")
  
  // Administration permissions
  val EditAllUsers = Value(7, "Edit All Users")
  val EditConfiguration = Value(8,"Edit Configuration")
}
