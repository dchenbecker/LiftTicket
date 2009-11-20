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

import _root_.scala.xml.{Node,NodeSeq,Text}

import _root_.net.liftweb.mapper.{AjaxEditableField,MappedField,Mapper}
import _root_.net.liftweb.util.{Helpers,Log}
import _root_.net.liftweb.http.{js,SHtml}
import js.{jquery,JsCmd,JsCmds,JE}
import jquery.JqJsCmds
import JsCmds.{Noop,SetHtml}
import JE.Str
import JqJsCmds.{Hide,Show}

/**
 * This trait can be added to an existing Mapper class to allow the entire mapper
 * to be displayed with ajax editable fields.
 */
trait AjaxEditableMapper[OwnerType <: Mapper[OwnerType]] extends Mapper[OwnerType] {
  self : OwnerType =>
  
  /**
   * This list defines the fields that will be displayed as well as whether they will
   * be editable. This allows you to mix editable and non-editable fields.
   */
  def displayFields : List[(MappedField[_,_],Boolean)]
  
  /** This method allows you to do programmatic control of whether the editable 
   *  fields will display as editable. The default is true */
  def editableFields = true
  
  /** This defines what content will wrap the fields in the display */
  def displayWrap (fields : NodeSeq) : NodeSeq = fields
  
  /** This defines the format for each individual field in the display */
  def displayField (label : NodeSeq, content : NodeSeq) : NodeSeq = 
    label ++ Text(" : ") ++ content ++ <br />
  
  /** This method is called when the instance's data are saved. The default is to do nothing */
  def onSave = {}
  
  override def asHtml = {
    val fieldContent : NodeSeq = 
      for ((field,editable) <- displayFields) yield {
        val content : NodeSeq = 
          if (editable && editableFields) {
            <xml:group>{
              field.toForm.map { form =>
                SHtml.ajaxEditable(field.asHtml, field.toForm.open_!, () => {this.save; onSave})
              } openOr field.asHtml
            }</xml:group>
          } else {
            field.asHtml
          }
          
        <xml:group>{displayField(Text(field.displayName),content)}</xml:group>
      }
    
    displayWrap(fieldContent)
  }
}

trait LoggedAjaxEditableField[FieldType,OwnerType <: Mapper[OwnerType]] extends AjaxEditableField[FieldType,OwnerType] {
  override def onSave {
    Log.info("Saved " + this.is)
  }
}
