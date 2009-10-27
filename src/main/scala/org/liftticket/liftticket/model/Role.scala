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
 * This class represents a role that aggregates permissions to simplify the
 * assignment of perms to users.
 */
class Role extends LongKeyedMapper[Role] with IdPK with OneToMany[Long,Role] {
  def getSingleton = Role
  
  object name extends MappedString(this, 200)
  object permissions extends MappedOneToMany(RolePermission,RolePermission.role)
  	with Owned[RolePermission] with Cascade[RolePermission]
}
object Role extends Role with LongKeyedMetaMapper[Role]

/**
 * A class that represents a permission for a role.
 */
class RolePermission extends LongKeyedMapper[RolePermission] with IdPK {
  def getSingleton = RolePermission
  object role extends MappedLongForeignKey(this,Role)
  object permission extends MappedEnum(this,Permissions)
}
object RolePermission extends RolePermission with LongKeyedMetaMapper[RolePermission]