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

import _root_.scala.xml.{NodeSeq,Text}

import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util.Helpers._

import liftticket.model._

import _root_.net.liftweb.mapper._

object RoleAdmin {  
  val listLoc = List("admin", "listRoles")
  val listPath = listLoc.mkString("/",  "/", "")
  
  val formLoc = List("admin", "createRole")
  val formPath = formLoc.mkString("/", "/", "")
  
  val delLoc = List("admin", "deleteRole")
  val delPath = delLoc.mkString("/", "/", "")
  
  object currentObject extends RequestVar[Box[Role]](Empty)
 
  // TODO: Break out a page for editing user membership on roles
  def list (xhtml : NodeSeq) : NodeSeq = {
    bind("content", xhtml,
         "title" -> Text(S.?("Roles")),
         "header" -> <tr><th>Name</th><th>Permissions</th><th>&nbsp;</th></tr>,
         "entries" -> Role.findAll.flatMap { role =>
           //val assignees = UserRole.find(By(UserRole.role, role.id),PreCache(UserRole.user)).map(_.user.obj)
           <tr>
             <td>{role.name.is}</td>
             <td>{role.permissions.all.flatMap{ perm => Text(perm.permission.toString) ++ <br/>}}</td>
             <td>{SHtml.link(formPath, () => currentObject(Full(role)), Text(S.?("Edit"))) ++ Text(" ") ++
                  SHtml.link(delPath, () => currentObject(Full(role)), Text(S.?("Delete")))}</td>
           </tr>
         },
         "additional" -> SHtml.link(formPath, () => currentObject(Full(Role.create)), Text(S.?("Add Role")))
    )
  }
  
  def createOrEdit (xhtml : NodeSeq) : NodeSeq = currentObject.is match {
    case Full(current) => {
      bind("content", xhtml,
    	   "body" -> {
             SHtml.hidden(() => currentObject(Full(current))) ++
             <tr>
               <th>Name</th>
               <td>{current.name.toForm openOr Text("Bad form create!")}</td>
             </tr>
             <tr>
               <th>Permissions</th>
               <td>{current.permissions.toForm}</td>
             </tr>
             <tr>
               <th>&nbsp;</th>
               <td>{ SHtml.submit(S.?("Save"), {() => current.save; S.redirectTo(listPath)})}</td>
             </tr>       
           }
      )
    }
    case _ => Text(S.?("No instance to edit"))
  }
  
  def delete (xhtml : NodeSeq) : NodeSeq = currentObject.is match {
    case Full(current) => {
      bind("content", xhtml,
           "body" -> {
             <p>Confirm deletetion of role {current.name.is}</p> ++
             SHtml.submit(S.?("Delete"), {() => current.delete_!; S.redirectTo(listPath)})
           }
      )
    }
    case _ => Text(S.?("No instance to delete"))
  }
  
  val missingTemplate = Text(S.?("Missing template"))
  
  def menus = {
    import Loc._
    Menu(Loc("listRoles", listLoc, S.?("Roles"), 
             Template(() => TemplateFinder.findAnyTemplate(List("templates-hidden", "list-template")) openOr missingTemplate), 
             Snippet("list", list), UserAdmin.canSeeAdminMenu)) ::
    Menu(Loc("editRole", formLoc, S.?("Edit Role"),
             Template(() => TemplateFinder.findAnyTemplate(List("templates-hidden", "create-template")) openOr missingTemplate),
    		 Snippet("create", createOrEdit), Hidden, UserAdmin.canSeeAdminMenu)) ::
    Menu(Loc("deleteRole", delLoc, S.?("Delete Role"),
             Template(() => TemplateFinder.findAnyTemplate(List("templates-hidden", "delete-template")) openOr missingTemplate),
             Snippet("delete", delete), Hidden, UserAdmin.canSeeAdminMenu)) :: Nil
  }
}
