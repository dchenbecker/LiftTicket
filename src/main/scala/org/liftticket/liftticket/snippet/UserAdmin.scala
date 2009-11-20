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

import _root_.scala.xml.NodeSeq

import _root_.net.liftweb.http.{S,SHtml,DispatchSnippet}
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.util.Helpers._

import liftticket.model._

/**
 * This object is responsible for all of the snippets and related methods for user
 * and user rights administration.
 */
object UserAdmin {
  private[liftticket] def canSeeAdminMenu () = {
    import Loc._
    
    If (() => {User.hasPermission(Permissions.EditAllUsers) ||
               Configuration.loggedInWithMasterPass.is}, S.?("org.liftticket.msgs.nopermission"))
  }
  
  // This method ties all of the User admin menus together
  def menus : List[Menu] = {
    import Loc._
    Menu(Loc("admin", List("admin","index"), S.?("User Administration"),canSeeAdminMenu), RoleAdmin.menus : _*) :: Nil
  } 
}
