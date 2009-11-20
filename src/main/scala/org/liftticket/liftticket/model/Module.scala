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
import _root_.net.liftweb.util.Log

/**
 * This entity represents a particular module or area of the system so that tickets
 * can be more easily classified.
 */
class Module extends LongKeyedMapper[Module] with IdPK with OneToMany[Long,Module] {
  def getSingleton = Module
  
  object name extends MappedString(this,200)
  object description extends MappedTextarea(this,4000) {
    override def textareaCols = 80
    override def textareaRows = 6
  }
  object releases extends MappedOneToMany(ModuleRelease,ModuleRelease.module)
}
object Module extends Module with LongKeyedMetaMapper[Module] {
  override def fieldOrder = id :: name :: description :: Nil 
}

/**
 * This entity represents a specific release of a particular module. Used to keep
 * track of versions to fix bugs against.
 */
class ModuleRelease extends LongKeyedMapper[ModuleRelease] with IdPK {
  def getSingleton = ModuleRelease
  
  object module extends MappedLongForeignKey(this,Module)
  object version extends MappedString(this,200)
}
object ModuleRelease extends ModuleRelease with LongKeyedMetaMapper[ModuleRelease]
