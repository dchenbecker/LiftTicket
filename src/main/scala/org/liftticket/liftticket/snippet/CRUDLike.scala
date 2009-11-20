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
import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.util._
import Helpers._

/**
 * This trait provides CRUD handling for Mappers that don't quite conform to
 * Lift's CRUDify trait expectations. The primary use case is Mappers that 
 * utilize OneToMany and ManyToMany, since these aren't actually MappedFields.
 * Instead, the implementing class/object can provide a list of
 * (name, (instance) => form, (instance) => display) elements to generate the
 * needed forms and display html.
 */
trait CRUDLike[KeyType, MapperType <: KeyedMetaMapper[KeyType,MapperType]] {
  self : MapperType =>
  
  /** This case class defines a single field and how it should be displayed 
   * 
   * @param label The label of the field, as it will be displayed
   * @param listDisplay_? Controls whether or not to display this field in
   * the list of all instances
   * @param viewDisplay_? Controls whether or not to display this field when 
   * viewing an instance
   * @param toForm A function that generates a form field for a given instance
   * @param toHtml A function that generates field display HTML for a given
   * instance
   */
  case class Field(label : NodeSeq,
                   listDisplay_? : Boolean,
                   viewDisplay_? : Boolean,
                   toForm : MapperType => NodeSeq,
                   toHtml : MapperType => NodeSeq)
  
  /** This defines the fields for this CRUDLike to work with */
  def fields : Seq[Field]
  
  /** Define the user-friendly name for a single instance*/
  def instanceName : String
  
  /** Define the plural name. 
   *  TODO: Not sure how to I18N this properly */
  def pluralName : String = instanceName + "s"
  
  /** The location of the list page */
  def listLoc = List("admin", "listRoles")
  
  /** The URI path of the list page */
  def listPath = listLoc.mkString("/",  "/", "")
  
  /** The location of the edit/create page */
  def formLoc = List("admin", "createRole")
  
  /** The URI path of the edit/create page */
  def formPath = formLoc.mkString("/", "/", "")
  
  /** The location of the delete page */
  def delLoc = List("admin", "deleteRole")
  
  /** The URI path of the delete page */
  def delPath = delLoc.mkString("/", "/", "")
  
  // This request var maintains the current object being worked on
  object currentObject extends RequestVar[Box[MapperType]](Empty)
  
  /** What to wrap the various pages in. Typically this is just the surround */
  def pageWrap (body : NodeSeq) : NodeSeq = 
    <lift:surround with="default" at="content">{body}</lift:surround>
}
