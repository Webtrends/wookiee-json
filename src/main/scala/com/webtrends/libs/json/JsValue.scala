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

import com.fasterxml.jackson.core.{JsonGenerator, JsonParser, JsonToken}
import com.webtrends.libs.ValidationError
import scala.collection._


case class JsResultException(errors: Seq[(JsPath, Seq[ValidationError])]) extends RuntimeException("JsResultException(errors:%s)".format(errors))

/**
  * Generic json value
  */
sealed trait JsValue extends JsReadable {
  override def toString = Json.stringify(this)

  def validate[A](implicit rds: Reads[A]): JsResult[A] = rds.reads(this)

  def validateOpt[A](implicit rds: Reads[A]): JsResult[Option[A]] = JsDefined(this).validateOpt[A]
}

object JsValue {
  import scala.language.implicitConversions
  implicit def jsValueToJsLookup(value: JsValue): JsLookup = JsLookup(JsDefined(value))
}

/**
  * Represents a Json null value.
  */
case object JsNull extends JsValue

/**
  * Represent a Json boolean value.
  */
case class JsBoolean(value: Boolean) extends JsValue

/**
  * Represent a Json number value.
  */
case class JsNumber(value: BigDecimal) extends JsValue

/**
  * Represent a Json string value.
  */
case class JsString(value: String) extends JsValue

/**
  * Represent a Json array value.
  */
case class JsArray(value: Seq[JsValue] = List()) extends JsValue {

  /**
    * Concatenates this array with the elements of an other array.
    */
  def ++(other: JsArray): JsArray =
    JsArray(value ++ other.value)

  /**
    * Append an element to this array.
    */
  def :+(el: JsValue): JsArray = JsArray(value :+ el)
  def append(el: JsValue): JsArray = this.:+(el)

  /**
    * Prepend an element to this array.
    */
  def +:(el: JsValue): JsArray = JsArray(el +: value)
  def prepend(el: JsValue): JsArray = this.+:(el)

}

/**
  * Represent a Json object value.
  */
case class JsObject(private val underlying: Map[String, JsValue]) extends JsValue {

  /**
    * The fields of this JsObject in the order passed to to constructor
    */
  lazy val fields: Seq[(String, JsValue)] = underlying.toSeq

  /**
    * The value of this JsObject as an immutable map.
    */
  lazy val value: Map[String, JsValue] = underlying match {
    case m: immutable.Map[String, JsValue] => m
    case m => m.toMap
  }

  /**
    * Return all fields as a set
    */
  def fieldSet: Set[(String, JsValue)] = fields.toSet

  /**
    * Return all keys
    */
  def keys: Set[String] = underlying.keySet

  /**
    * Return all values
    */
  def values: Iterable[JsValue] = underlying.values

  /**
    * Merge this object with another one. Values from other override value of the current object.
    */
  def ++(other: JsObject): JsObject = JsObject(underlying ++ other.underlying)

  /**
    * Removes one field from the JsObject
    */
  def -(otherField: String): JsObject = JsObject(underlying - otherField)

  /**
    * Adds one field to the JsObject
    */
  def +(otherField: (String, JsValue)): JsObject = JsObject(underlying + otherField)

  /**
    * merges everything in depth and doesn't stop at first level, as ++ does
    */
  def deepMerge(other: JsObject): JsObject = {
    def merge(existingObject: JsObject, otherObject: JsObject): JsObject = {
      val result = existingObject.underlying ++ otherObject.underlying.map {
        case (otherKey, otherValue) =>
          val maybeExistingValue = existingObject.underlying.get(otherKey)

          val newValue = (maybeExistingValue, otherValue) match {
            case (Some(e: JsObject), o: JsObject) => merge(e, o)
            case _ => otherValue
          }
          otherKey -> newValue
      }
      JsObject(result)
    }
    merge(this, other)
  }

  override def equals(other: Any): Boolean = other match {
    case that: JsObject => (that canEqual this) && fieldSet == that.fieldSet
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[JsObject]

  override def hashCode: Int = fieldSet.hashCode()
}

object JsObject {
  /**
    * Construct a new JsObject, with the order of fields in the Seq.
    */
  def apply(fields: Seq[(String, JsValue)]): JsObject = new JsObject(mutable.LinkedHashMap(fields: _*))
}