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

import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.mapper.{KeyedMapper,Mapper}

/**
 * Mixing this trait into a mapper makes the asHtml method return a table
 */
trait TableDisplay[KeyType, OwnerType <: KeyedMapper[KeyType,OwnerType]] 
    extends KeyedMapper[KeyType, OwnerType] with AjaxEditableMapper[OwnerType] {
  self : OwnerType =>
  
  override def displayWrap(fields : NodeSeq) = <table>{fields}</table>
    
  override def displayField(label : NodeSeq, content : NodeSeq) = 
    <tr><th>{label}</th><td>{content}</td></tr>
}
