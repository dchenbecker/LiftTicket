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

import scala.xml.{NodeSeq,Text}

import net.liftweb.mapper._

import net.liftweb.http.SHtml

import net.liftweb.common.Full

import net.liftweb.util.Log

/**
 * For now we'll just build on MegaProtoUser. At some point we may need to
 * rip this out and build our own entity from scratch, but let's keep things
 * simple to start. 
 */
class User extends MegaProtoUser[User] with OneToMany[Long,User] with ManyToMany {
  def getSingleton = User

  private def thisUser = this
    
  object permissions extends MappedOneToMany(UserPermission, UserPermission.user) 
      with Owned[UserPermission] with Cascade[UserPermission] {
    def toForm : NodeSeq = {
  	  val currentChoices = this.all.map(_.permission.is.id.toString).toSeq 
        
      val allChoices = Permissions.elements.toList.map {perm => (perm.id.toString,perm.toString)}
        
      SHtml.multiSelect(allChoices, currentChoices, 
                        { selected : List[String] =>
                          thisUser.save // Force parent save so that there's a PK for the one-to-many
                          this.clear
                          selected.foreach { item =>
                            val newPerm = UserPermission.create.permission(Permissions(item.toInt))
                            this += newPerm
                          }
                        })
  	}
    
    private def permEntry (perm : UserPermission) : NodeSeq = Text(perm.permission.toString) ++ <br/>
    private def permEntry (perm : RolePermission) : NodeSeq = Text("*" + perm.permission.toString) ++ <br/>
    
    def asHtml = this.all.flatMap(permEntry) ++
      roles.all.flatMap{ role => role.permissions.all.flatMap(permEntry) }
  }
  
  object roles extends MappedManyToMany(UserRole, UserRole.user, UserRole.role, Role) {
    def toForm : NodeSeq = {
  	  val currentChoices = this.all.map(_.id.toString).toSeq 
        
      val allChoices = Role.findAll.map {role => (role.id.toString,role.name.is)}
        
      SHtml.multiSelect(allChoices, currentChoices, 
                        { selected : List[String] =>
                          thisUser.save // Force parent save so that there's a PK for the many-to-many
                          this.clear
                          selected.foreach { item =>
                            Role.find(item) match {
                              case Full(role) => this += role
                              case _ => Log.error("Could not locate role for user perms editing : " + item)
                            }
                          }
                        })
  	}
    
    def asHtml = this.all.flatMap{ role => Text(role.name.toString) ++ <br/>}
  }
  
  def hasPermission(perm : Permissions.Value) = permissions.all.exists(_.permission == perm) ||
    roles.all.exists(_.permissions.all.exists(_.permission == perm))
}

/**
 * Define the singleton for our User entity. Nothing special here, just some
 * custom retrieval methods.
 */
object User extends User with MetaMegaProtoUser[User] with CRUDOps[Long,User] {
  override def screenWrap = 
    Full(<lift:surround with="default" at="content"><div id="formBox"><lift:bind /></div></lift:surround>)

  val instanceName = "User"
  
  override def calcFields = 
    List(firstName, lastName, email, locale, timezone).map(defaultField) :::
    Field(password.displayNameHtml openOr Text("Password"), true, false, _.password.toForm openOr Text(""), _.password.asHtml) ::
    Field(Text("Validated"), true, true, _.validated.toForm openOr Text(""), _.validated.asHtml) ::
    Field(Text("Permissions"), true, true, _.permissions.toForm, _.permissions.asHtml) ::
    Field(Text("Roles"), true, true, _.roles.toForm, _.roles.asHtml) :: Nil
  
  override def hasPermission(perm : Permissions.Value) =
    currentUser.map(_.hasPermission(perm)) openOr false
  
  override def menus = super[CRUDOps].menus
}

/**
 * A class that represents a permission for a specific user. Specific perms
 * take priority over Role permissions (they act as an override in case of
 * conflict). Roles have their own permission composition class.
 */
class UserPermission extends LongKeyedMapper[UserPermission] with IdPK {
  def getSingleton = UserPermission
  object user extends MappedLongForeignKey(this,User)
  object permission extends MappedEnum(this,Permissions)
}
object UserPermission extends UserPermission with LongKeyedMetaMapper[UserPermission]

/**
 * A class to represent a many-to-many on User that defines
 * what roles a user may be in. A role is a simple way of aggregating perms.
 */
class UserRole extends LongKeyedMapper[UserRole] with IdPK {
  def getSingleton = UserRole
  object role extends MappedLongForeignKey(this, Role)
  object user extends MappedLongForeignKey(this, User)
}
object UserRole extends UserRole with LongKeyedMetaMapper[UserRole]
