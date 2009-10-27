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
 * For now we'll just build on MegaProtoUser. At some point we may need to
 * rip this out and build our own entity from scratch, but let's keep things
 * simple to start. 
 */
class User extends MegaProtoUser[User] with OneToMany[Long,User] with ManyToMany {
  def getSingleton = User
  
  object permissions extends MappedOneToMany(UserPermission, UserPermission.user) 
    with Owned[UserPermission] with Cascade[UserPermission]
    
  object roles extends MappedManyToMany(UserRole, UserRole.user, UserRole.role, Role)
}

/**
 * Define the singleton for our User entity. Nothing special here, just some
 * custom retrieval methods.
 */
object User extends User with MetaMegaProtoUser[User] {
  
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