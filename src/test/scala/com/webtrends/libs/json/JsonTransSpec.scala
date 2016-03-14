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

import com.webtrends.libs.ValidationError
import org.junit.runner.RunWith
import org.specs2.mutable._
import com.webtrends.libs.json.Json._
import com.webtrends.libs.json.Reads._
import com.webtrends.harness.functional.syntax._
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object JsonTransSpec extends Specification {
  "JSON transformers " should {
    val js = Json.obj(
      "field1" -> "alpha",
      "field2" -> 123L,
      "field3" -> Json.obj(
        "field31" -> "beta",
        "field32"-> 345
      ),
      "field4" -> Json.arr("alpha", 2, true, Json.obj("field41" -> "toto", "field42" -> "tata"))
    )

    "pick a value at a path" in {
      js.transform(
        (__ \ 'field3).json.pick
      ).get must beEqualTo(
        Json.obj(
          "field31" -> "beta", "field32"-> 345
        )
      )
    }

    "pick a branch" in {
      js.transform(
        (__ \ 'field3).json.pickBranch
      ).get must beEqualTo(
        Json.obj(
          "field3" -> Json.obj("field31" -> "beta", "field32"-> 345)
        )
      )
    }

    "copy input JSON and update a branch (merge the updated branch with input JSON)" in {
      js.transform(
        (__ \ 'field3).json.update(
          __.read[JsObject].map{ o => o ++ Json.obj( "field33" -> false ) }
        )
      ).get must beEqualTo(
        Json.obj(
          "field1" -> "alpha",
          "field2" -> 123L,
          "field3" -> Json.obj(
            "field31" -> "beta",
            "field32" -> 345,
            "field33" -> false
          ),
          "field4" -> Json.arr("alpha", 2, true, Json.obj("field41" -> "toto", "field42" -> "tata"))
        )
      )
    }

    "pick a branch and update its content" in {
      js.transform(
        (__ \ 'field3).json.pickBranch(
          (__ \ 'field32).json.update(
            Reads.of[JsNumber].map{ case JsNumber(nb) => JsNumber(nb + 12) }
          ) andThen
          (__ \ 'field31).json.update(
            Reads.of[JsString].map{ case JsString(s) => JsString(s + "toto") }
          )
        )
      ).get must beEqualTo(
        Json.obj(
          "field3" -> Json.obj("field31" -> "betatoto", "field32"-> 357)
        )
      )
    }

    "put a value in a new branch (don't keep passed json)" in {
      js.transform(
        (__ \ 'field3).json.put(JsNumber(234))
      ).get must beEqualTo(
        Json.obj(
          "field3" -> 234
        )
      )
    }

    "create a new path by copying a branch" in {
      js.transform(
        (__ \ 'field5).json.copyFrom( (__ \ 'field3).json.pick )
      ).get must beEqualTo(
        Json.obj(
          "field5" -> Json.obj(
            "field31" -> "beta",
            "field32" -> 345
          )
        )
      )
    }

    "copy full json and prune a branch" in {
      js.transform(
        (__ \ 'field3).json.prune
      ).get must beEqualTo(
        Json.obj(
          "field1" -> "alpha",
          "field2" -> 123L,
          "field4" -> Json.arr("alpha", 2, true, Json.obj("field41" -> "toto", "field42" -> "tata"))
        )
      )
    }

    "pick a single branch and prune a sub-branch" in {
      js.transform(
        (__ \ 'field3).json.pickBranch(
          (__ \ 'field32).json.prune
        )
      ).get must beEqualTo(
        Json.obj(
          "field3" -> Json.obj("field31" -> "beta")
        )
      )
    }

    "copy the full json and update a 2nd-level path and then prune a subbranch" in {
      js.validate(
        (__ \ 'field3 \ 'field32).json.update(
          Reads.of[JsNumber].map{ case JsNumber(nb) => JsNumber(nb + 5) }
        ) andThen (__ \ 'field4).json.prune
      ).get must beEqualTo(
        Json.obj(
          "field1" -> "alpha",
          "field2" -> 123L,
          "field3" -> Json.obj(
            "field31" -> "beta",
            "field32"-> 350
          )
        )
      )
    }

    "deepMerge when reducing JsObjects" in {
      val json = Json.obj("somekey1" -> 11, "somekey2" -> 22)
      val jsonTransform = (
        (__ \ "key1" \ "sk1").json.copyFrom((__ \ "somekey1").json.pick)
        and
        (__ \ "key1" \ "sk2").json.copyFrom((__ \ "somekey2").json.pick)
      ).reduce

      json.validate(jsonTransform).get must beEqualTo(
        Json.obj("key1" -> Json.obj("sk1" -> 11, "sk2" -> 22))
      )
    }

    "report the correct path in the JsError" in {
      "when the field to modify doesn't exist" in {
        js.transform(
          (__ \ 'field42).json.update(__.read[JsString])
        ).asEither.left.get.head must_== (__ \ 'field42, Seq(ValidationError("error.path.missing")))
      }

      "when the reader is the wrong type" in {
        js.transform(
          (__ \ 'field2).json.update(__.read[JsString])
        ).asEither.left.get.head must_== (__ \ 'field2, Seq(ValidationError("error.expected.jsstring")))
      }
    }
  }
}
