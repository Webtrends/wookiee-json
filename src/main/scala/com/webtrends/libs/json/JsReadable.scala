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

package com.webtrends.libs.json

/**
  * A trait representing a Json node which can be read as an arbitrary type A using a Reads[A]
  */
trait JsReadable extends Any {
  /**
    * Tries to convert the node into a T. An implicit Reads[T] must be defined.
    * Any error is mapped to None
    *
    * @return Some[T] if it succeeds, None if it fails.
    */
  def asOpt[T](implicit fjs: Reads[T]): Option[T] = validate(fjs).asOpt

  /**
    * Tries to convert the node into a T, throwing an exception if it can't. An implicit Reads[T] must be defined.
    */
  def as[T](implicit fjs: Reads[T]): T = validate(fjs).fold(
    valid = identity,
    invalid = e => throw new JsResultException(e)
  )

  /**
    * Transforms this node into a JsResult using provided Json transformer Reads[JsValue]
    */
  def transform[A <: JsValue](rds: Reads[A]): JsResult[A] = validate(rds)

  /**
    * Tries to convert the node into a JsResult[T] (Success or Error). An implicit Reads[T] must be defined.
    */
  def validate[T](implicit rds: Reads[T]): JsResult[T]
}