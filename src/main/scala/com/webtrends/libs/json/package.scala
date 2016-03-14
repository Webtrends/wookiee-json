/*
 *  Copyright (c) 2016 Webtrends (http://www.webtrends.com)
 *  See the LICENCE.txt file distributed with this work for additional
 *  information regarding copyright ownership.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.webtrends.libs

/**
 * Json API
 * For example:
 * {{{
 *  case class User(id: Long, name: String, friends: List[User])
 *
 *  implicit object UserFormat extends Format[User] {
 *   def reads(json: JsValue): User = User(
 *     (json \ "id").as[Long],
 *     (json \ "name").as[String],
 *     (json \ "friends").asOpt[List[User]].getOrElse(List()))
 *   def writes(u: User): JsValue = JsObject(List(
 *     "id" -> JsNumber(u.id),
 *     "name" -> JsString(u.name),
 *     "friends" -> JsArray(u.friends.map(fr => JsObject(List("id" -> JsNumber(fr.id),
 *     "name" -> JsString(fr.name)))))))
 * }
 *
 * }}}
 */
package object json {

  /**
   * Alias for `JsPath` companion object
   */
  val __ = JsPath

}
