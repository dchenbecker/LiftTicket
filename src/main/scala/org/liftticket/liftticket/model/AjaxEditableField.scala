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

import _root_.net.liftweb.mapper.{MappedField,Mapper}
import _root_.net.liftweb.util.{Helpers,Log}
import _root_.net.liftweb.http.{js,SHtml}
import js.{jquery,JsCmd,JsCmds,JE}
import jquery.JqJsCmds
import JsCmds.{Noop,SetHtml}
import JE.Str
import JqJsCmds.{Hide,Show}

/**
 * This object defines some AJAX helper methods to simplify form handling.
 */
object AjaxUtils {
  /**
   * This method generates an AJAX editable field. Normally, the displayContents
   * will be shown, with an "Edit" button. If the "Edit" button is clicked, the field
   * will be replaced with the edit form, along with an "OK" and "Cancel" button. 
   * If the OK button is pressed, the form fields are submitted and the onSubmit
   * function is called, and then the displayContents are re-run to get a new display.
   * If cancel is pressed then the original displayContents are re-shown.
   */
  def editable (displayContents : => NodeSeq, editForm : => NodeSeq, onSubmit : () => Unit) : NodeSeq = {
	val divName = Helpers.nextFuncName
	val dispName = divName + "_display"
	val editName = divName + "_edit"
 
	def swapJsCmd (show : String, hide : String) : JsCmd = Show(show) & Hide(hide) 
 
    def setAndSwap (show : String, showContents : => NodeSeq, hide : String) : JsCmd =
      (SHtml.ajaxCall(Str("ignore"), {ignore : String => SetHtml(show, showContents)})._2.cmd & swapJsCmd(show,hide))
      
    def displayMarkup : NodeSeq = 
      displayContents ++ Text(" ") ++ 
      <input value="Edit" type="button" onclick={setAndSwap(editName, editMarkup, dispName).toJsCmd + " return false;"} /> 
    
    def editMarkup : NodeSeq = {
      val formData = 
        editForm ++ 
        <input type="submit" value="OK" /> ++ 
        SHtml.hidden(onSubmit) ++
        <input type="button" onclick={swapJsCmd(dispName,editName).toJsCmd + " return false;"} value="Cancel" />
      
      SHtml.ajaxForm(formData, 
                     Noop, 
                     setAndSwap(dispName, displayMarkup, editName))
    }
      
    <div>
      <div id={dispName}>
        {displayMarkup}
      </div>
      <div id={editName} style="display: none;">
        {editMarkup}
      </div>
    </div>    
  }
}
/**
 * This trait can be added to existing Mapper fields to make them use AjaxUtils.editable
 * for field display.
 */
trait AjaxEditableField[FieldType,OwnerType <: Mapper[OwnerType]] extends MappedField[FieldType,OwnerType] {
  override def asHtml : Node = 
    <xml:group>{
      toForm.map { form =>
      	AjaxUtils.editable(super.asHtml, toForm.open_!, () => {fieldOwner.save; onSave})
      } openOr super.asHtml
    }</xml:group>
  
  /** This method is called when the element's data are saved. The default is to do nothing */
  def onSave {}
}

trait LoggedAjaxEditableField[FieldType,OwnerType <: Mapper[OwnerType]] extends AjaxEditableField[FieldType,OwnerType] {
  override def onSave {
    Log.info("Saved " + this.is)
  }
}
