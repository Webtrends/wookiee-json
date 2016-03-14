
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
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object JsObjectSpec extends Specification {

  "JsObject.deepMerge" should {
    "not fail when the objects are empty" in {
      Json.obj().deepMerge(Json.obj()) must beEqualTo(Json.obj())
    }

    "merge correctly when the source object is empty" in {
      val populatedObj = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull
      )
      populatedObj.deepMerge(Json.obj()) must beEqualTo(populatedObj)
    }

    "merge correctly when the incoming object is empty" in {
      val populatedObj = Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull
      )
      Json.obj().deepMerge(populatedObj) must beEqualTo(populatedObj)
    }
  }

  "JsObject.deepMerge should keep existing attributes where there is no collision and" should {

    "overwrite existing attributes on collision when value is not a JsArray or JsObject" in {
      Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> JsNull,
        "field4" -> 456,
        "field5" -> "abc",
        "field6" -> "def"
      ).deepMerge(Json.obj(
        "field4" -> 789,
        "field5" -> "xyz",
        "field6" -> JsNull
      )) must beEqualTo(
        Json.obj(
          "field1" -> 123,
          "field2" -> "abc",
          "field3" -> JsNull,
          "field4" -> 789,
          "field5" -> "xyz",
          "field6" -> JsNull
        )
      )
    }

    "recursively merge where elements are both of type JsArray or both of type JsObject" in {
      Json.obj(
        "field1" -> 123,
        "field2" -> "abc",
        "field3" -> Json.arr(
          "abc", "def", "ghi"
        ),
        "field4" -> Json.obj(
          "field1a" -> 888,
          "field2b" -> "xxx",
          "field3c" -> JsNull
        )
      ).deepMerge(Json.obj(
        "field3" -> Json.arr(
          "jkl", "mno", "pqr"
        ),
        "field4" -> Json.obj(
          "field1a" -> 999,
          "field2b" -> "yyy",
          "field3c" -> "zzz"
        )
      )) must beEqualTo(
        Json.obj(
          "field1" -> 123,
          "field2" -> "abc",
          "field3" -> Json.arr(
            "jkl", "mno", "pqr"
          ),
          "field4" -> Json.obj(
            "field1a" -> 999,
            "field2b" -> "yyy",
            "field3c" -> "zzz"
          )
        )
      )
    }

    "properly merge a deep structure" in {
      Json.obj(
        "field1a" -> Json.obj(
          "field2a" -> Json.obj(
            "field3a" -> Json.obj(
              "field4a" -> Json.obj(
                "field5a" -> "abc",
                "field5b" -> Json.arr("111", "222"),
                "field5d" -> Json.arr(Json.obj("a" -> 1), Json.obj("b" -> 2))
              )
            ),
            "field2b" -> Json.arr("aaa", "bbb"),
            "field2c" -> Json.obj(
              "hello" -> "world"
            )
          ),
          "field2b" -> "xxx",
          "field2c" -> JsNull
        )
      ).deepMerge(Json.obj(
        "field1a" -> Json.obj(
          "field2a" -> Json.obj(
            "field3a" -> Json.obj(
              "field4a" -> Json.obj(
                "field5b" -> Json.arr("333", "444"),
                "field5c" -> "deep",
                "field5d" -> Json.arr(Json.obj("c" -> 3), Json.obj("d" -> 4))
              )
            ),
            "field2b" -> Json.arr("ccc", "ddd"),
            "field2c" -> Json.obj(
              "hello" -> "new world"
            )
          ),
          "field2b" -> "yyy",
          "field2d" -> "zzz"
        )
      )) must beEqualTo(
        Json.obj(
          "field1a" -> Json.obj(
            "field2a" -> Json.obj(
              "field3a" -> Json.obj(
                "field4a" -> Json.obj(
                  "field5a" -> "abc",
                  "field5b" -> Json.arr("333", "444"),
                  "field5c" -> "deep",
                  "field5d" -> Json.arr(Json.obj("c" -> 3), Json.obj("d" -> 4))
                )
              ),
              "field2b" -> Json.arr("ccc", "ddd"),
              "field2c" -> Json.obj(
                "hello" -> "new world"
              )
            ),
            "field2b" -> "yyy",
            "field2c" -> JsNull,
            "field2d" -> "zzz"
          )
        )
      )
    }
  }
}
