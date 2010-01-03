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

import _root_.scala.xml.{NodeSeq,Text}

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.common.{Box,Empty,Full}
import _root_.net.liftweb.http.SHtml

import liftticket.snippet.UserAdmin

/**
 * This class represents a role that aggregates permissions to simplify the
 * assignment of perms to users.
 */
class Role extends LongKeyedMapper[Role] with IdPK with OneToMany[Long,Role] {
  def getSingleton = Role
  
  object name extends MappedString(this, 200) {
    override def displayName = "Name"
  }
  private def thisRole = this
  object permissions extends MappedOneToMany(RolePermission,RolePermission.role)
  	with Owned[RolePermission] with Cascade[RolePermission] {
  	  def toForm : NodeSeq = {
  	    val currentChoices = this.all.map(_.permission.is.id.toString).toSeq 
        
        val allChoices = Permissions.elements.toList.map {perm => (perm.id.toString,perm.toString)}
        
        SHtml.multiSelect(allChoices, currentChoices, { selected : List[String] =>
                            thisRole.save
                            this.clear
                            selected.foreach { item =>
                              val newPerm = RolePermission.create.permission(Permissions(item.toInt))
                              this += newPerm
                            }
                          })
  	  }
     
      def asHtml : NodeSeq = this.all.flatMap{ perm => Text(perm.permission.toString) ++ <br/>}
  	}
}
object Role extends Role with LongKeyedMetaMapper[Role] with CRUDOps[Long,Role] {
  val instanceName = "Role"
  
  override def calcFields = 
    defaultField(name) :: 
    Field(Text("Permissions"), true, true, _.permissions.toForm, _.permissions.asHtml) :: Nil
  
  override def calcCheckPermissions = {
    case _ => CRUDPermissions(UserAdmin.canSeeAdminMenuTest)
  }
}

/**
 * A class that represents a permission for a role.
 */
class RolePermission extends LongKeyedMapper[RolePermission] with IdPK {
  def getSingleton = RolePermission
  object role extends MappedLongForeignKey(this,Role)
  object permission extends MappedEnum(this,Permissions)
}
object RolePermission extends RolePermission with LongKeyedMetaMapper[RolePermission]
