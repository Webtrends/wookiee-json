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

import org.junit.runner.RunWith
import org.specs2.mutable._
import com.webtrends.libs.json.Json._
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object JsonRichSpec extends Specification {

  "JSON" should {
    "create json with rich syntax" in {
      val js = Json.obj(
            "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
            "key2" -> 123,
            "key3" -> true,
            "key4" -> Json.arr("value41", 345.6, JsString("test"), JsObject(Seq("key411" -> obj("key4111" -> 987.654))))
          )

       js must equalTo( 
       		JsObject(Seq(
            	"key1" -> JsObject(Seq(
                	"key11" -> JsString("value11"),
                  "key12" -> JsNumber(123L),
                  "key13" -> JsNull
            	)),
            	"key2" -> JsNumber(123),
            	"key3" -> JsBoolean(true),
              "key4" -> JsArray(Seq(
                          JsString("value41"), JsNumber(345.6), 
                          JsString("test"), JsObject(Seq("key411" -> JsObject(Seq("key4111" -> JsNumber(987.654)))))
                        ))
          ))
       )

    }
  }

}

