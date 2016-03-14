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
  * A validation error.
  *
  * @param messages the error message, if more then one message is passed it will use the last one
  * @param args the error message arguments
  */
case class ValidationError(messages: Seq[String], args: Any*) {

  lazy val message = messages.last

}

object ValidationError {

  def apply(message: String, args: Any*) = new ValidationError(Seq(message), args: _*)

}
