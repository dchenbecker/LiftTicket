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
import net.liftweb.util.{Helpers,Log}
import Helpers._
import net.liftweb.sitemap.Loc
import net.liftweb.http.{S,SHtml}
import net.liftweb.common._

import liftticket.snippet.UserAdmin

/**
 * This entity represents a particular module or area of the system so that tickets
 * can be more easily classified.
 */
class Module extends LongKeyedMapper[Module] with IdPK with OneToMany[Long,Module] {
  def getSingleton = Module
  
  object name extends MappedString(this,200) {
    override def displayName = "Name"
  }
  object description extends MappedTextarea(this,4000) {
    override def textareaCols = 80
    override def textareaRows = 6
    override def displayName = "Description"
  }
  object releases extends MappedOneToMany(ModuleRelease,ModuleRelease.module)
}
object Module extends Module with LongKeyedMetaMapper[Module] with CRUDOps[Long,Module] {
   val instanceName = "Module"
   
   def ifAdmin (body : NodeSeq) : NodeSeq = 
     if (UserAdmin.canSeeAdminMenuTest) { body } else <xml:group />
   
   override def calcFields = 
     defaultField(name) ::
     defaultField(description) ::
     Field(Text("Releases"), {instance => 
       instance.releases.all.flatMap{mr : ModuleRelease => 
         Text(mr.version.is) ++ Text(" ") ++
         ifAdmin(SHtml.link(ModuleRelease.crudEditPath, () => ModuleRelease.currentObject(Full(mr)), Text(S.?("Edit"))) ++ Text(" ")) ++
         ifAdmin(SHtml.link(ModuleRelease.crudDeletePath, () => ModuleRelease.currentObject(Full(mr)), Text(S.?("Delete")))) ++
         <br/>
       } ++
       ifAdmin(SHtml.link(ModuleRelease.crudEditPath, () => ModuleRelease.currentObject(Full(ModuleRelease.create.module(instance))), Text(S.?("Add"))))
       }) :: Nil
   
  // Restrict non-view ops
  override def calcCheckPermissions = {
    import CrudOperations._    
    { case Create | Delete | Edit => CRUDPermissions(UserAdmin.canSeeAdminMenuTest) }
  }
}

/**
 * This entity represents a specific release of a particular module. Used to keep
 * track of versions to fix bugs against.
 */
class ModuleRelease extends LongKeyedMapper[ModuleRelease] with IdPK {
  def getSingleton = ModuleRelease
  
  object module extends MappedLongForeignKey(this,Module)
  object version extends MappedString(this,200) {
    override def displayName = "Version"
  }
  // Notes and other things about this particular release
  object notes extends MappedTextarea(this,4000) {
    override def displayName = "Notes"
    override def textareaCols = 80
    override def textareaRows = 6
  }
}
object ModuleRelease extends ModuleRelease with LongKeyedMetaMapper[ModuleRelease] with CRUDOps[Long,ModuleRelease] {
  val instanceName = "Release"
  
  override def calcFields = (version :: notes :: Nil).map(defaultField)
  
  // Restrict non-view ops
  override def calcCheckPermissions = {
    import CrudOperations._
    { 
      case Delete | Edit => CRUDPermissions(UserAdmin.canSeeAdminMenuTest)
      /* 
       * We want people to be able to create releases, but only via the Module CRUD
       * pages, not via a menu or link.
       */
      case Create => CRUDPermissions(UserAdmin.canSeeAdminMenuTest, false)
      // No listing of releases. That's all done via the Module
      case ViewAll => CRUDPermissions(true,false)
    }
  }
  
  override def postSaveRedirectPath = Module.crudListPath
  override def postDeleteRedirectPath = Module.crudListPath
}
