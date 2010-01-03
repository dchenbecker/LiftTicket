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

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.sitemap._
import net.liftweb.util._
import Helpers._

/** This enumeration defines the possible operations for CRUD */
object CrudOperations extends Enumeration {
  val Create,View,Edit,Delete,ViewAll = Value
}

/**
 * This is a utility class to encapsulate permissions for accessing and viewing pages.
 */
class CRUDPermissions(val access : Boolean, val view : Boolean)
  
/**
 * Helper object for CRUDPermissions to simplify creation.
 */
object CRUDPermissions {
  /** Creates a CRUDPermissions instance with the specified values. */
  def apply (access : Boolean, view : Boolean) = new CRUDPermissions(access, view)
  
  /** Creates a CRUDPermissions instance with the specified value for both access and view. */
  def apply (accessAndView : Boolean) = new CRUDPermissions(accessAndView, accessAndView)
}

/**
 * This trait is intended to be a flexible way to mix CRUD operations into
 * an existing MetaMapper. In addition to normal CRUD on the MetaMapper's 
 * defined fields, this allows you to create synthetic fields and simplifies
 * control over access to various functions.
 */
trait CRUDOps[KeyType, MapperType <: KeyedMapper[KeyType,MapperType]] {
  self : MapperType with KeyedMetaMapper[KeyType,MapperType] =>
  
  /**
   * This class encapsulates the CRUD control and generation info for a given
   * CRUD "field". This allows completely synthetic fields outside of the 
   * standard Mapper types.
   * 
   * @param label The label that will be used in view and list output
   * @param editDisplay_? Whether to display this field in edit pages
   * @param viewDisplay_? Whether to display this field in view pages
   * @param listDisplay_? Whether to display this field in list pages
   * @param toForm A method that will generate field form markup for a given instance
   * @param toViewHtml A method that will generate field view markup for a given instance
   * @param toListHtml A method that will generate field list markup for a given instance
   */
  class Field(val label : NodeSeq,
              val editDisplay_? : Boolean,
              val viewDisplay_? : Boolean,
              val listDisplay_? : Boolean,
              val toForm : MapperType => NodeSeq,
              val toViewHtml : MapperType => NodeSeq,
              val toListHtml : MapperType => NodeSeq)
  
  /**
   * Appliers for Field to simplify definition
   */
  object Field {
    /** This method can be used as a NOOP for converter functions */
    def noop(ignore : MapperType) : NodeSeq = Text("")

    /** Defines a display-only field that uses the same HTML for view and list */
    def apply(label : NodeSeq,
              toHtml : MapperType => NodeSeq) =
      new Field(label, false, true, true, noop, toHtml, toHtml)
    
    /** Defines a read-write field that uses the same HTML for view and list */
    def apply(label : NodeSeq,
              editDisplay_? : Boolean,
              viewDisplay_? : Boolean,
              toForm : MapperType => NodeSeq,
              toHtml : MapperType => NodeSeq) =
      new Field(label, editDisplay_?, viewDisplay_?, viewDisplay_?, toForm, toHtml, toHtml)
    
    def apply(label : NodeSeq,
              editDisplay_? : Boolean,
              viewDisplay_? : Boolean,
              listDisplay_? : Boolean,
              toForm : MapperType => NodeSeq,
              toViewHtml : MapperType => NodeSeq,
              toListHtml : MapperType => NodeSeq) =
      new Field(label, editDisplay_?, viewDisplay_?, listDisplay_?, toForm, toViewHtml, toListHtml)
  }
  
  /** This calculates the fields for this CRUDOps to work with */
  def calcFields : Seq[Field] = mappedFields.map(defaultField)
  
  /** The fields that this CRUDOps works with */
  private lazy val fields : Seq[Field] = calcFields
  
  /** A default generator for a field instance with control over view/edit/list 
   */
  def defaultField (field : BaseMappedField,
                    edit : Boolean,
                    view : Boolean,
                    list : Boolean) : Field = {
    val displayFunc = { 
      instance : MapperType => (instance.fieldByName(field.name).map{mf : MappedField[_,MapperType] => mf.asHtml}) openOr Text("Could not generate form for " + field.name)                 
    }
    
    Field(Text(field.displayName),
          edit, view, list,
          { instance => 
            (instance.fieldByName(field.name).flatMap{mf : MappedField[_,MapperType] => mf.toForm}) openOr Text("Could not generate form for " + field.name)},
          displayFunc,
          displayFunc
    )
  }

  
  /**
   * A default Field generator based on Mapper fields
   */
  def defaultField (field : BaseMappedField) : Field =
    defaultField(field,
                 field.dbIncludeInForm_?,
                 field.dbDisplay_?,
                 field.dbDisplay_?)
    
  /** Define the user-friendly name for a single instance*/
  def instanceName : String
  
  /** Define the plural name. 
   *  TODO: Not sure how to I18N this properly */
  def pluralName : String = instanceName + "s"
  
  /* What is the path prefix for our crud ops? Uses the instance name */
  private lazy val locPrefix = instanceName
  
  /** The location of the list page */
  def listLoc = List(locPrefix, "list")
  
  /** The URI path of the list page */
  lazy val crudListPath = listLoc.mkString("/",  "/", "")
  
  /** The location of the view instance page */
  def viewLoc = List(locPrefix, "view")
  
  /** The URI of the view instance page */
  lazy val crudViewPath = viewLoc.mkString("/", "/", "")
  
  /** The location of the create instance page */
  def createLoc = List(locPrefix, "create")
  
  /** The URI of the create instance page */
  lazy val crudCreatePath = createLoc.mkString("/", "/", "")
  
  /** The location of the edit page */
  def editLoc = List(locPrefix, "edit")
  
  /** The URI path of the edit page */
  lazy val crudEditPath = editLoc.mkString("/", "/", "")
  
  /** The location of the delete page */
  def deleteLoc = List(locPrefix, "delete")
  
  /** The URI path of the delete page */
  lazy val crudDeletePath = deleteLoc.mkString("/", "/", "")
  
  // This request var maintains the current object being worked on
  object currentObject extends RequestVar[Box[MapperType]](Empty)
  
  /** What to wrap the various pages in. Typically this is just the surround
   * @param body What to wrap
   */
  def pageWrap (body : NodeSeq) : NodeSeq = 
    <lift:surround with="default" at="content">{body}</lift:surround>
    
  def listTemplate : NodeSeq = 
    <lift:list>
      <h1>{pluralName}:</h1>
      <table>
        <content:header/>
        <content:entries/>
      </table>
      <content:additional />
    </lift:list>
          
  def formTemplate : NodeSeq = 
    <lift:edit form="POST">
      <obj:current />
      <table>
      <obj:fields>
        <tr><th><field:label /></th><td><field:entry /></td></tr>
      </obj:fields>
      <obj:ops />&nbsp;<obj:cancel />
      </table>
    </lift:edit>
    
  def viewTemplate : NodeSeq = formTemplate
    
  /**
   * This method may be overridden to control which instances are shown in 
   * the list view.
   */
  def getListInstances : List[MapperType] = findAll
    
  def list (xhtml : NodeSeq) : NodeSeq = {
    val listFields = fields.filter(_.listDisplay_?)
    
    // TODO: Is there a cleaner/more scala way to do the checkPermissions transforms?
    bind("content", xhtml,
         "header" -> <tr>{ 
           listFields.flatMap {field => <th>{field.label}</th>} ++
           (if (checkView(CrudOperations.View)) { <th>&nbsp;</th> } else Text("")) ++  
           (if (checkView(CrudOperations.Edit)) { <th>&nbsp;</th> } else Text("")) ++
           (if (checkView(CrudOperations.Delete)) { <th>&nbsp;</th> } else Text(""))
         }</tr>,
         "entries" -> getListInstances.flatMap { instance =>
           <tr>{
             listFields.flatMap { field => <td>{field.toListHtml(instance)}</td>} ++
             (if (checkView(CrudOperations.View)) {
               <td>{SHtml.link(crudViewPath, {() => currentObject(Full(instance))}, Text(S.?("View")))}</td>
             } else Text("")) ++
             (if (checkView(CrudOperations.Edit)) {
               <td>{SHtml.link(crudEditPath, {() => currentObject(Full(instance))}, Text(S.?("Edit")))}</td>
             } else Text("")) ++
             (if (checkView(CrudOperations.Delete)) {
               <td>{SHtml.link(crudDeletePath, {() => currentObject(Full(instance))}, Text(S.?("Delete")))}</td>
             } else Text(""))
           }</tr>
         },
         "additional" -> (
           if (checkView(CrudOperations.Create)) {
             <lift:Menu.item name={createMenuName} />
           } else {
             Text("")
           }
         )
    )
  }
    
  /* This is a generator function that will bind the specified template to
   * each field that matches the predicate using the generator to construct the
   * bound markup.
   */
  private def fieldMap(template : NodeSeq,
                       instance : MapperType, 
                       predicate : Field => Boolean, 
                       generator : Field => (MapperType => NodeSeq)) : NodeSeq = {
    fields.filter(predicate).flatMap({ field =>
      bind("field", template, "label" -> field.label, "entry" -> generator(field)(instance))
    })
  }
    
  /** This defines the message when someone attemps to delete or edit a non-existant instance */
  def invalidInstanceTemplate : NodeSeq = Text(S.?("Invalid " + instanceName))
  
  /** Controls where to go after an edit/create */
  def postSaveRedirectPath = crudListPath
  
  def edit (xhtml : NodeSeq) : NodeSeq = currentObject.is match {
    case Full(current) => {
      bind("obj", xhtml,
           "current" -> SHtml.hidden(() => currentObject(Full(current))),
           "fields" -> fieldMap(chooseTemplate("obj", "fields", xhtml), current, {_.editDisplay_?}, {_.toForm}),
           "ops" -> SHtml.submit(S.?("Save"), () => if (current.save) S.redirectTo(postSaveRedirectPath)),
           "cancel" -> <input type="button" value={S.?("Cancel")} onclick="history.back();" />
      )
    }
    case _ => invalidInstanceTemplate
  }
  
  /** Controls where to go after an edit/create */
  def postDeleteRedirectPath = crudListPath
  
  def delete (xhtml : NodeSeq) : NodeSeq = currentObject.is match {
    case Full(current) => {
      bind("obj", xhtml,
           "current" -> Text(S.?("Confirm deletion")),
           "fields" -> fieldMap(chooseTemplate("obj", "fields", xhtml), current, {_.viewDisplay_?}, {_.toViewHtml}),
           "ops" -> SHtml.submit(S.?("Delete"), {() => current.delete_!; S.redirectTo(postDeleteRedirectPath)}),
           "cancel" -> <input type="button" value={S.?("Cancel")} onclick="history.back();" />
      )
    }
    case _ => invalidInstanceTemplate
  }
  
  def view (xhtml : NodeSeq) : NodeSeq = currentObject.is match {
    case Full(current) => {
      bind("obj", xhtml,
           "current" -> Text(""),
           "fields" -> fieldMap(chooseTemplate("obj", "fields", xhtml), current, {_.viewDisplay_?}, {_.toViewHtml}),
           "ops" -> Text(""), // TODO: Put edit, delete ops here
           "cancel" -> <input type="button" value={S.?("Back")} onclick="history.back();" />
      )
    }
    case _ => invalidInstanceTemplate
  } 
  
  /** 
   * You may override this method to do programmatic control of access to 
   * the various operations. The default is to allow everything. The return 
   * value is a Pair(allow access, show menu item). Show menu item is only
   * valid for list and create menus. All other menus are automatically hidden
   * since they require operation on a single instance.
   */
  def calcCheckPermissions : PartialFunction[CrudOperations.Value,CRUDPermissions] = {
    case _ => CRUDPermissions(true)
  }
  
  // We cache the function here for later use. This is here specifically so that
  // someone can't have a missing match.
  private lazy val checkPermissions : CrudOperations.Value => CRUDPermissions = 
    calcCheckPermissions orElse {
      case _ => CRUDPermissions(true)
    }

  /** You may override this to change what error message users are shown when they
      try to access a restricted page. */
  def noPermissionsMessage (operation : CrudOperations.Value) : String = {
    S.?("You do not have permission to view this page")
  }
  
  // Helper to get access condition
  private def checkAccess(op : CrudOperations.Value) : Boolean =
    checkPermissions(op).access
  
  // Helper to get view condition
  private def checkView(op : CrudOperations.Value) : Boolean = 
    checkPermissions(op).view
 
  private def listMenu : Menu = {
    import Loc._
    
    // We're creating a custom loc so that we can control when the menu is shown independent of access to it
    val listLocItem = new Loc[Unit] {
      val defaultParams = Empty
      val defaultValue = Full(())
      val name = "list" + pluralName
      val text = new LinkText({ignore : Unit => Text(S.?(pluralName))})
      val link = Link(listLoc, false, crudListPath)
      override def params = 
        Template(() => pageWrap(listTemplate)) ::
        Snippet("list", list) :: 
        If(() => checkAccess(CrudOperations.ViewAll), noPermissionsMessage(CrudOperations.ViewAll)) :: Nil
      override def hidden = ! checkView(CrudOperations.ViewAll)
    }
    
    Menu(listLocItem)
  }
 
  // This is used to link to the create menu item as needed
  private lazy val createMenuName = "create" + instanceName
  
  private def createMenu : Menu = {
    import Loc._
    
    // We're creating a custom loc so that we can control when the menu is shown independent of access to it
    val createLocItem = new Loc[Unit] {
      val defaultParams = Empty
      val defaultValue = Full(())
      val name = createMenuName
      val text = new LinkText({ignore : Unit => Text(S.?("Create " + instanceName))})
      val link = Link(createLoc, false, crudCreatePath)
      override def params = 
        Template({() => 
          currentObject(Full(getSingleton.create)) ; pageWrap(formTemplate)
        }) ::
        Snippet("edit", edit) :: 
        If(() => checkAccess(CrudOperations.Create), noPermissionsMessage(CrudOperations.Create)) :: Nil
      override def hidden = ! checkView(CrudOperations.Create)
    }
    
    Menu(createLocItem)
  }
    
  // TODO: Add rewrite to allow for direct view based on ID if desired
  private def viewMenu : Menu = {
    import Loc._
    Menu(Loc("view" + instanceName, viewLoc, S.?("View  " + instanceName),
             (Template(() => pageWrap(viewTemplate)) ::
              Snippet("edit", view) ::
              If(() => checkAccess(CrudOperations.View), noPermissionsMessage(CrudOperations.View)) ::                     
              Hidden :: Nil) : _*))
  }
  
  private def editMenu : Menu = {
    import Loc._
    Menu(Loc("edit" + instanceName, editLoc, S.?("Edit " + instanceName),
             (Template(() => pageWrap(formTemplate)) :: Snippet("edit", edit) :: 
              If(() => checkAccess(CrudOperations.Edit), noPermissionsMessage(CrudOperations.Edit)) ::
              Hidden :: Nil) : _*))
  }
  
  private def deleteMenu : Menu = {
    import Loc._
    Menu(Loc("delete" + instanceName, deleteLoc, S.?("Delete " + instanceName),
             (Template(() => pageWrap(formTemplate)) :: Snippet("edit", delete) :: 
              If(() => checkAccess(CrudOperations.Delete), noPermissionsMessage(CrudOperations.Delete)) ::  
              Hidden :: Nil) : _*))
  }
  
  def menus : List[Menu] = List(deleteMenu, editMenu, viewMenu, listMenu, createMenu)
}
