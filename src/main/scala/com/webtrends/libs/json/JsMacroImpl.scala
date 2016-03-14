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

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

object JsMacroImpl {
  def readsImpl[A: c.WeakTypeTag](c: Context): c.Expr[Reads[A]] = {
    import c.universe._
    import c.universe.Flag._

    val companioned = weakTypeOf[A].typeSymbol
    val companionSymbol = companioned.companionSymbol
    val companionType = companionSymbol.typeSignature

    val libsPkg = Select(Select(Ident(TermName("com")), TermName("webtrends")), TermName("libs"))
    val jsonPkg = Select(libsPkg, TermName("json"))
    val libsHarnessPkg = Select(Select(Ident(TermName("com")), TermName("webtrends")), TermName("harness"))
    val functionalSyntaxPkg = Select(Select(libsHarnessPkg, TermName("functional")), TermName("syntax"))
    val utilPkg = Select(jsonPkg, TermName("util"))

    val jsPathSelect = Select(jsonPkg, TermName("JsPath"))
    val readsSelect = Select(jsonPkg, TermName("Reads"))
    val unliftIdent = Select(functionalSyntaxPkg, TermName("unlift"))
    val lazyHelperSelect = Select(utilPkg, TypeName("LazyHelper"))

    companionType.decl(TermName("unapply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No unapply function found")
      case s =>
        val unapply = s.asMethod
        val unapplyReturnTypes = unapply.returnType match {
          case TypeRef(_, _, Nil) =>
            c.abort(c.enclosingPosition, s"Apply of ${companionSymbol} has no parameters. Are you using an empty case class?")
          case TypeRef(_, _, args) =>
            args.head match {
              case t @ TypeRef(_, _, Nil) => Some(List(t))
              case t @ TypeRef(_, _, args) =>
                if (t <:< typeOf[Option[_]]) Some(List(t))
                else if (t <:< typeOf[Seq[_]]) Some(List(t))
                else if (t <:< typeOf[Set[_]]) Some(List(t))
                else if (t <:< typeOf[Map[_, _]]) Some(List(t))
                else if (t <:< typeOf[Product]) Some(args)
              case _ => None
            }
          case _ => None
        }

        //println("Unapply return type:" + unapply.returnType)

        companionType.decl(TermName("apply")) match {
          case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
          case s =>
            // searches apply method corresponding to unapply
            val applies = s.asMethod.alternatives
            val apply = applies.collectFirst {
              case (apply: MethodSymbol) if (apply.paramss.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes) => apply
            }
            apply match {
              case Some(apply) =>
                //println("apply found:" + apply)    
                val params = apply.paramss.head //verify there is a single parameter group

                val inferedImplicits = params.map(_.typeSignature).map { implType =>

                  val (isRecursive, tpe) = implType match {
                    case TypeRef(_, t, args) =>
                      // Option[_] needs special treatment because we need to use XXXOpt
                      if (implType.typeConstructor <:< typeOf[Option[_]].typeConstructor)
                        (args.exists { a => a.typeSymbol == companioned }, args.head)
                      else (args.exists { a => a.typeSymbol == companioned }, implType)
                    case TypeRef(_, t, _) =>
                      (false, implType)
                  }

                  // builds reads implicit from expected type
                  val neededImplicitType = appliedType(weakTypeOf[Reads[_]].typeConstructor, tpe :: Nil)
                  // infers implicit
                  val neededImplicit = c.inferImplicitValue(neededImplicitType)
                  (implType, neededImplicit, isRecursive, tpe)
                }

                // if any implicit is missing, abort
                // else goes on
                inferedImplicits.collect { case (t, impl, rec, _) if (impl == EmptyTree && !rec) => t } match {
                  case List() =>
                    val namedImplicits = params.map(_.name).zip(inferedImplicits)
                    //println("Found implicits:"+namedImplicits)

                    val helperMember = Select(This(tpnme.EMPTY), TermName("lazyStuff"))

                    var hasRec = false

                    // combines all reads into CanBuildX
                    val canBuild = namedImplicits.map {
                      case (name, (t, impl, rec, tpe)) =>
                        // inception of (__ \ name).read(impl)
                        val jspathTree = Apply(
                          Select(jsPathSelect, TermName(scala.reflect.NameTransformer.encode("\\"))),
                          List(Literal(Constant(name.decoded)))
                        )

                        if (!rec) {
                          val readTree =
                            if (t.typeConstructor <:< typeOf[Option[_]].typeConstructor)
                              Apply(
                                Select(jspathTree, TermName("readNullable")),
                                List(impl)
                              )
                            else Apply(
                              Select(jspathTree, TermName("read")),
                              List(impl)
                            )

                          readTree
                        } else {
                          hasRec = true
                          val readTree =
                            if (t.typeConstructor <:< typeOf[Option[_]].typeConstructor)
                              Apply(
                                Select(jspathTree, TermName("readNullable")),
                                List(
                                  Apply(
                                    Select(Apply(jsPathSelect, List()), TermName("lazyRead")),
                                    List(helperMember)
                                  )
                                )
                              )

                            else {
                              Apply(
                                Select(jspathTree, TermName("lazyRead")),
                                if (tpe.typeConstructor <:< typeOf[List[_]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(readsSelect, TermName("list")),
                                    List(helperMember)
                                  )
                                )
                                else if (tpe.typeConstructor <:< typeOf[Set[_]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(readsSelect, TermName("set")),
                                    List(helperMember)
                                  )
                                )
                                else if (tpe.typeConstructor <:< typeOf[Seq[_]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(readsSelect, TermName("seq")),
                                    List(helperMember)
                                  )
                                )
                                else if (tpe.typeConstructor <:< typeOf[Map[_, _]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(readsSelect, TermName("map")),
                                    List(helperMember)
                                  )
                                )
                                else List(helperMember)
                              )
                            }

                          readTree
                        }
                    }.reduceLeft { (acc, r) =>
                      Apply(
                        Select(acc, TermName("and")),
                        List(r)
                      )
                    }

                    // builds the final Reads using apply method
                    val applyMethod =
                      Function(
                        params.foldLeft(List[ValDef]())((l, e) =>
                          l :+ ValDef(Modifiers(PARAM), TermName(e.name.encoded), TypeTree(), EmptyTree)
                        ),
                        Apply(
                          Select(Ident(companionSymbol.name), TermName("apply")),
                          params.foldLeft(List[Tree]())((l, e) =>
                            l :+ Ident(TermName(e.name.encoded))
                          )
                        )
                      )

                    val unapplyMethod = Apply(
                      unliftIdent,
                      List(
                        Select(Ident(companionSymbol.name), unapply.name)
                      )
                    )

                    // if case class has one single field, needs to use inmap instead of canbuild.apply
                    val finalTree = if (params.length > 1) {
                      Apply(
                        Select(canBuild, TermName("apply")),
                        List(applyMethod)
                      )
                    } else {
                      Apply(
                        Select(canBuild, TermName("map")),
                        List(applyMethod)
                      )
                    }
                    //println("finalTree: "+finalTree)

                    if (!hasRec) {
                      val block = Block(
                        Import(functionalSyntaxPkg, List(ImportSelector(nme.WILDCARD, -1, null, -1))),
                        finalTree
                      )

                      //println("block:"+block)

                      /*val reif = reify(
                        /*new com.webtrends.libs.json.util.LazyHelper[Format, A] {
                          override lazy val lazyStuff: Format[A] = null
                        }*/
                      )
                      println("RAW:"+showRaw(reif.tree, printKinds = true))*/

                      c.Expr[Reads[A]](block)
                    } else {
                      val helper = TermName("helper")
                      val helperVal = ValDef(
                        Modifiers(),
                        helper,
                        TypeTree(weakTypeOf[com.webtrends.libs.json.util.LazyHelper[Reads, A]]),
                        Apply(lazyHelperSelect, List(finalTree))
                      )

                      val block = Select(
                        Block(
                          Import(functionalSyntaxPkg, List(ImportSelector(nme.WILDCARD, -1, null, -1))),
                          ClassDef(
                            Modifiers(Flag.FINAL),
                            TypeName("$anon"),
                            List(),
                            Template(
                              List(
                                AppliedTypeTree(
                                  lazyHelperSelect,
                                  List(
                                    Ident(weakTypeOf[Reads[A]].typeSymbol),
                                    Ident(weakTypeOf[A].typeSymbol)
                                  )
                                )
                              ),
                              emptyValDef,
                              List(
                                DefDef(
                                  Modifiers(),
                                  nme.CONSTRUCTOR,
                                  List(),
                                  List(List()),
                                  TypeTree(),
                                  Block(
                                    Apply(
                                      Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
                                      List()
                                    )
                                  )
                                ),
                                ValDef(
                                  Modifiers(Flag.OVERRIDE | Flag.LAZY),
                                  TermName("lazyStuff"),
                                  AppliedTypeTree(Ident(weakTypeOf[Reads[A]].typeSymbol), List(TypeTree(weakTypeOf[A]))),
                                  finalTree
                                )
                              )
                            )
                          ),
                          Apply(Select(New(Ident(TypeName("$anon"))), nme.CONSTRUCTOR), List())
                        ),
                        TermName("lazyStuff")
                      )

                      //println("block:"+block)

                      c.Expr[Reads[A]](block)
                    }
                  case l => c.abort(c.enclosingPosition, s"No implicit Reads for ${l.mkString(", ")} available.")
                }

              case None => c.abort(c.enclosingPosition, "No apply function found matching unapply return types")
            }

        }
    }
  }

  def writesImpl[A: c.WeakTypeTag](c: Context): c.Expr[Writes[A]] = {
    import c.universe._
    import c.universe.Flag._

    val companioned = weakTypeOf[A].typeSymbol
    val companionSymbol = companioned.companionSymbol
    val companionType = companionSymbol.typeSignature

    val libsPkg = Select(Select(Ident(TermName("com")), TermName("webtrends")), TermName("libs"))
    val libsHarnessPkg = Select(Select(Ident(TermName("com")), TermName("webtrends")), TermName("harness"))
    val jsonPkg = Select(libsPkg, TermName("json"))
    val functionalSyntaxPkg = Select(Select(libsHarnessPkg, TermName("functional")), TermName("syntax"))
    val utilPkg = Select(jsonPkg, TermName("util"))

    val jsPathSelect = Select(jsonPkg, TermName("JsPath"))
    val writesSelect = Select(jsonPkg, TermName("Writes"))
    val unliftIdent = Select(functionalSyntaxPkg, TermName("unlift"))
    val lazyHelperSelect = Select(utilPkg, TypeName("LazyHelper"))

    companionType.declaration(TermName("unapply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No unapply function found")
      case s =>
        val unapply = s.asMethod
        val unapplyReturnTypes = unapply.returnType match {
          case TypeRef(_, _, Nil) =>
            c.abort(c.enclosingPosition, s"Unapply of ${companionSymbol} has no parameters. Are you using an empty case class?")
          case TypeRef(_, _, args) =>
            args.head match {
              case t @ TypeRef(_, _, Nil) => Some(List(t))
              case t @ TypeRef(_, _, args) =>
                if (t <:< typeOf[Option[_]]) Some(List(t))
                else if (t <:< typeOf[Seq[_]]) Some(List(t))
                else if (t <:< typeOf[Set[_]]) Some(List(t))
                else if (t <:< typeOf[Map[_, _]]) Some(List(t))
                else if (t <:< typeOf[Product]) Some(args)
              case _ => None
            }
          case _ => None
        }

        //println("Unapply return type:" + unapplyReturnTypes)

        companionType.declaration(TermName("apply")) match {
          case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
          case s =>
            // searches apply method corresponding to unapply
            val applies = s.asMethod.alternatives
            val apply = applies.collectFirst {
              case (apply: MethodSymbol) if (apply.paramss.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes) => apply
            }
            apply match {
              case Some(apply) =>
                //println("apply found:" + apply)    
                val params = apply.paramss.head //verify there is a single parameter group

                val inferedImplicits = params.map(_.typeSignature).map { implType =>

                  val (isRecursive, tpe) = implType match {
                    case TypeRef(_, t, args) =>
                      // Option[_] needs special treatment because we need to use XXXOpt
                      if (implType.typeConstructor <:< typeOf[Option[_]].typeConstructor)
                        (args.exists { a => a.typeSymbol == companioned }, args.head)
                      else (args.exists { a => a.typeSymbol == companioned }, implType)
                    case TypeRef(_, t, _) =>
                      (false, implType)
                  }

                  // builds reads implicit from expected type
                  val neededImplicitType = appliedType(weakTypeOf[Writes[_]].typeConstructor, tpe :: Nil)
                  // infers implicit
                  val neededImplicit = c.inferImplicitValue(neededImplicitType)
                  (implType, neededImplicit, isRecursive, tpe)
                }

                // if any implicit is missing, abort
                // else goes on
                inferedImplicits.collect { case (t, impl, rec, _) if (impl == EmptyTree && !rec) => t } match {
                  case List() =>
                    val namedImplicits = params.map(_.name).zip(inferedImplicits)
                    //println("Found implicits:"+namedImplicits)

                    val helperMember = Select(This(tpnme.EMPTY), TermName("lazyStuff"))

                    var hasRec = false

                    // combines all reads into CanBuildX
                    val canBuild = namedImplicits.map {
                      case (name, (t, impl, rec, tpe)) =>
                        // inception of (__ \ name).read(impl)
                        val jspathTree = Apply(
                          Select(jsPathSelect, TermName(scala.reflect.NameTransformer.encode("\\"))),
                          List(Literal(Constant(name.decoded)))
                        )

                        if (!rec) {
                          val writesTree =
                            if (t.typeConstructor <:< typeOf[Option[_]].typeConstructor)
                              Apply(
                                Select(jspathTree, TermName("writeNullable")),
                                List(impl)
                              )
                            else Apply(
                              Select(jspathTree, TermName("write")),
                              List(impl)
                            )

                          writesTree
                        } else {
                          hasRec = true
                          val writesTree =
                            if (t.typeConstructor <:< typeOf[Option[_]].typeConstructor)
                              Apply(
                                Select(jspathTree, TermName("writeNullable")),
                                List(
                                  Apply(
                                    Select(Apply(jsPathSelect, List()), TermName("lazyWrite")),
                                    List(helperMember)
                                  )
                                )
                              )

                            else {
                              Apply(
                                Select(jspathTree, TermName("lazyWrite")),
                                if (tpe.typeConstructor <:< typeOf[List[_]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(writesSelect, TermName("list")),
                                    List(helperMember)
                                  )
                                )
                                else if (tpe.typeConstructor <:< typeOf[Set[_]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(writesSelect, TermName("set")),
                                    List(helperMember)
                                  )
                                )
                                else if (tpe.typeConstructor <:< typeOf[Seq[_]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(writesSelect, TermName("seq")),
                                    List(helperMember)
                                  )
                                )
                                else if (tpe.typeConstructor <:< typeOf[Map[_, _]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(writesSelect, TermName("map")),
                                    List(helperMember)
                                  )
                                )
                                else List(helperMember)
                              )
                            }

                          writesTree
                        }
                    }.reduceLeft { (acc, r) =>
                      Apply(
                        Select(acc, TermName("and")),
                        List(r)
                      )
                    }

                    // builds the final Reads using apply method
                    //val applyMethod = Ident( companionSymbol.name )
                    val applyMethod =
                      Function(
                        params.foldLeft(List[ValDef]())((l, e) =>
                          l :+ ValDef(Modifiers(PARAM), TermName(e.name.encoded), TypeTree(), EmptyTree)
                        ),
                        Apply(
                          Select(Ident(companionSymbol.name), TermName("apply")),
                          params.foldLeft(List[Tree]())((l, e) =>
                            l :+ Ident(TermName(e.name.encoded))
                          )
                        )
                      )

                    val unapplyMethod = Apply(
                      unliftIdent,
                      List(
                        Select(Ident(companionSymbol.name), unapply.name)
                      )
                    )

                    // if case class has one single field, needs to use inmap instead of canbuild.apply
                    val finalTree = if (params.length > 1) {
                      Apply(
                        Select(canBuild, TermName("apply")),
                        List(unapplyMethod)
                      )
                    } else {
                      Apply(
                        Select(canBuild, TermName("contramap")),
                        List(unapplyMethod)
                      )
                    }
                    //println("finalTree: "+finalTree)

                    if (!hasRec) {
                      val block = Block(
                        Import(functionalSyntaxPkg, List(ImportSelector(nme.WILDCARD, -1, null, -1))),
                        finalTree
                      )
                      //println("block:"+block)
                      c.Expr[Writes[A]](block)
                    } else {
                      val helper = TermName("helper")
                      val helperVal = ValDef(
                        Modifiers(),
                        helper,
                        TypeTree(weakTypeOf[com.webtrends.libs.json.util.LazyHelper[Writes, A]]),
                        Apply(lazyHelperSelect, List(finalTree))
                      )

                      val block = Select(
                        Block(
                          Import(functionalSyntaxPkg, List(ImportSelector(nme.WILDCARD, -1, null, -1))),
                          ClassDef(
                            Modifiers(Flag.FINAL),
                            TypeName("$anon"),
                            List(),
                            Template(
                              List(
                                AppliedTypeTree(
                                  lazyHelperSelect,
                                  List(
                                    Ident(weakTypeOf[Writes[A]].typeSymbol),
                                    Ident(weakTypeOf[A].typeSymbol)
                                  )
                                )
                              ),
                              emptyValDef,
                              List(
                                DefDef(
                                  Modifiers(),
                                  nme.CONSTRUCTOR,
                                  List(),
                                  List(List()),
                                  TypeTree(),
                                  Block(
                                    Apply(
                                      Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
                                      List()
                                    )
                                  )
                                ),
                                ValDef(
                                  Modifiers(Flag.OVERRIDE | Flag.LAZY),
                                  TermName("lazyStuff"),
                                  AppliedTypeTree(Ident(weakTypeOf[Writes[A]].typeSymbol), List(TypeTree(weakTypeOf[A]))),
                                  finalTree
                                )
                              )
                            )
                          ),
                          Apply(Select(New(Ident(TypeName("$anon"))), nme.CONSTRUCTOR), List())
                        ),
                        TermName("lazyStuff")
                      )

                      //println("block:"+block)

                      /*val reif = reify(
                        new com.webtrends.libs.json.util.LazyHelper[Format, A] {
                          override lazy val lazyStuff: Format[A] = null
                        }
                      )
                      //println("RAW:"+showRaw(reif.tree, printKinds = true))*/
                      c.Expr[Writes[A]](block)
                    }
                  case l => c.abort(c.enclosingPosition, s"No implicit Writes for ${l.mkString(", ")} available.")
                }

              case None => c.abort(c.enclosingPosition, "No apply function found matching unapply parameters")
            }

        }
    }
  }

  def formatImpl[A: c.WeakTypeTag](c: Context): c.Expr[Format[A]] = {
    import c.universe._
    import c.universe.Flag._

    val companioned = weakTypeOf[A].typeSymbol
    val companionSymbol = companioned.companionSymbol
    val companionType = companionSymbol.typeSignature

    val libsPkg = Select(Select(Ident(TermName("com")), TermName("webtrends")), TermName("libs"))
    val jsonPkg = Select(libsPkg, TermName("json"))
    val libsHarnessPkg = Select(Select(Ident(TermName("com")), TermName("webtrends")), TermName("harness"))
    val functionalSyntaxPkg = Select(Select(libsHarnessPkg, TermName("functional")), TermName("syntax"))
    val utilPkg = Select(jsonPkg, TermName("util"))

    val jsPathSelect = Select(jsonPkg, TermName("JsPath"))
    val readsSelect = Select(jsonPkg, TermName("Reads"))
    val writesSelect = Select(jsonPkg, TermName("Writes"))
    val unliftIdent = Select(functionalSyntaxPkg, TermName("unlift"))
    val lazyHelperSelect = Select(utilPkg, TypeName("LazyHelper"))

    companionType.decl(TermName("unapply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No unapply function found")
      case s =>
        val unapply = s.asMethod
        val unapplyReturnTypes = unapply.returnType match {
          case TypeRef(_, _, Nil) =>
            c.abort(c.enclosingPosition, s"Unapply of ${companionSymbol} has no parameters. Are you using an empty case class?")
          case TypeRef(_, _, args) =>
            args.head match {
              case t @ TypeRef(_, _, Nil) => Some(List(t))
              case t @ TypeRef(_, _, args) =>
                if (t <:< typeOf[Option[_]]) Some(List(t))
                else if (t <:< typeOf[Seq[_]]) Some(List(t))
                else if (t <:< typeOf[Set[_]]) Some(List(t))
                else if (t <:< typeOf[Map[_, _]]) Some(List(t))
                else if (t <:< typeOf[Product]) Some(args)
              case _ => None
            }
          case _ => None
        }

        //println("Unapply return type:" + unapplyReturnTypes)

        companionType.decl(TermName("apply")) match {
          case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
          case s =>
            // searches apply method corresponding to unapply
            val applies = s.asMethod.alternatives
            val apply = applies.collectFirst {
              case (apply: MethodSymbol) if (apply.paramss.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes) => apply
            }
            apply match {
              case Some(apply) =>
                //println("apply found:" + apply)    
                val params = apply.paramss.head //verify there is a single parameter group

                val inferedImplicits = params.map(_.typeSignature).map { implType =>

                  val (isRecursive, tpe) = implType match {
                    case TypeRef(_, t, args) =>
                      // Option[_] needs special treatment because we need to use XXXOpt
                      if (implType.typeConstructor <:< typeOf[Option[_]].typeConstructor)
                        (args.exists { a => a.typeSymbol == companioned }, args.head)
                      else (args.exists { a => a.typeSymbol == companioned }, implType)
                    case TypeRef(_, t, _) =>
                      (false, implType)
                  }

                  // builds reads implicit from expected type
                  val neededImplicitType = appliedType(weakTypeOf[Format[_]].typeConstructor, tpe :: Nil)
                  // infers implicit
                  val neededImplicit = c.inferImplicitValue(neededImplicitType)
                  (implType, neededImplicit, isRecursive, tpe)
                }

                // if any implicit is missing, abort
                // else goes on
                inferedImplicits.collect { case (t, impl, rec, _) if (impl == EmptyTree && !rec) => t } match {
                  case List() =>
                    val namedImplicits = params.map(_.name).zip(inferedImplicits)
                    //println("Found implicits:"+namedImplicits)

                    val helperMember = Select(This(tpnme.EMPTY), TermName("lazyStuff"))

                    var hasRec = false

                    // combines all reads into CanBuildX
                    val canBuild = namedImplicits.map {
                      case (name, (t, impl, rec, tpe)) =>
                        // inception of (__ \ name).read(impl)
                        val jspathTree = Apply(
                          Select(jsPathSelect, TermName(scala.reflect.NameTransformer.encode("\\"))),
                          List(Literal(Constant(name.decoded)))
                        )

                        if (!rec) {
                          val formatTree =
                            if (t.typeConstructor <:< typeOf[Option[_]].typeConstructor)
                              Apply(
                                Select(jspathTree, TermName("formatNullable")),
                                List(impl)
                              )
                            else Apply(
                              Select(jspathTree, TermName("format")),
                              List(impl)
                            )

                          formatTree
                        } else {
                          hasRec = true
                          val formatTree =
                            if (t.typeConstructor <:< typeOf[Option[_]].typeConstructor)
                              Apply(
                                Select(jspathTree, TermName("formatNullable")),
                                List(
                                  Apply(
                                    Select(Apply(jsPathSelect, List()), TermName("lazyFormat")),
                                    List(helperMember)
                                  )
                                )
                              )

                            else {
                              Apply(
                                Select(jspathTree, TermName("lazyFormat")),
                                if (tpe.typeConstructor <:< typeOf[List[_]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(readsSelect, TermName("list")),
                                    List(helperMember)
                                  ),
                                  Apply(
                                    Select(writesSelect, TermName("list")),
                                    List(helperMember)
                                  )
                                )
                                else if (tpe.typeConstructor <:< typeOf[Set[_]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(readsSelect, TermName("set")),
                                    List(helperMember)
                                  ),
                                  Apply(
                                    Select(writesSelect, TermName("set")),
                                    List(helperMember)
                                  )
                                )
                                else if (tpe.typeConstructor <:< typeOf[Seq[_]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(readsSelect, TermName("seq")),
                                    List(helperMember)
                                  ),
                                  Apply(
                                    Select(writesSelect, TermName("seq")),
                                    List(helperMember)
                                  )
                                )
                                else if (tpe.typeConstructor <:< typeOf[Map[_, _]].typeConstructor)
                                  List(
                                  Apply(
                                    Select(readsSelect, TermName("map")),
                                    List(helperMember)
                                  ),
                                  Apply(
                                    Select(writesSelect, TermName("map")),
                                    List(helperMember)
                                  )
                                )
                                else List(helperMember)
                              )
                            }

                          formatTree
                        }
                    }.reduceLeft { (acc, r) =>
                      Apply(
                        Select(acc, TermName("and")),
                        List(r)
                      )
                    }

                    // builds the final Reads using apply method
                    //val applyMethod = Ident( companionSymbol.name )
                    val applyMethod =
                      Function(
                        params.foldLeft(List[ValDef]())((l, e) =>
                          l :+ ValDef(Modifiers(PARAM), TermName(e.name.encoded), TypeTree(), EmptyTree)
                        ),
                        Apply(
                          Select(Ident(companionSymbol.name), TermName("apply")),
                          params.foldLeft(List[Tree]())((l, e) =>
                            l :+ Ident(TermName(e.name.encoded))
                          )
                        )
                      )

                    val unapplyMethod = Apply(
                      unliftIdent,
                      List(
                        Select(Ident(companionSymbol.name), unapply.name)
                      )
                    )

                    // if case class has one single field, needs to use inmap instead of canbuild.apply
                    val finalTree = if (params.length > 1) {
                      Apply(
                        Select(canBuild, TermName("apply")),
                        List(applyMethod, unapplyMethod)
                      )
                    } else {
                      Apply(
                        Select(canBuild, TermName("inmap")),
                        List(applyMethod, unapplyMethod)
                      )
                    }
                    //println("finalTree: "+finalTree)

                    if (!hasRec) {
                      val block = Block(
                        Import(functionalSyntaxPkg, List(ImportSelector(nme.WILDCARD, -1, null, -1))),
                        finalTree
                      )
                      //println("block:"+block)
                      c.Expr[Format[A]](block)
                    } else {
                      val helper = TermName("helper")
                      val helperVal = ValDef(
                        Modifiers(),
                        helper,
                        Ident(weakTypeOf[com.webtrends.libs.json.util.LazyHelper[Format, A]].typeSymbol),
                        Apply(Ident(TermName("LazyHelper")), List(finalTree))
                      )

                      val block = Select(
                        Block(
                          Import(functionalSyntaxPkg, List(ImportSelector(nme.WILDCARD, -1, null, -1))),
                          ClassDef(
                            Modifiers(Flag.FINAL),
                            TypeName("$anon"),
                            List(),
                            Template(
                              List(
                                AppliedTypeTree(
                                  lazyHelperSelect,
                                  List(
                                    Ident(weakTypeOf[Format[A]].typeSymbol),
                                    Ident(weakTypeOf[A].typeSymbol)
                                  )
                                )
                              ),
                              emptyValDef,
                              List(
                                DefDef(
                                  Modifiers(),
                                  nme.CONSTRUCTOR,
                                  List(),
                                  List(List()),
                                  TypeTree(),
                                  Block(
                                    Apply(
                                      Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
                                      List()
                                    )
                                  )
                                ),
                                ValDef(
                                  Modifiers(Flag.OVERRIDE | Flag.LAZY),
                                  TermName("lazyStuff"),
                                  AppliedTypeTree(Ident(weakTypeOf[Format[A]].typeSymbol), List(TypeTree(weakTypeOf[A]))),
                                  finalTree
                                )
                              )
                            )
                          ),
                          Apply(Select(New(Ident(TypeName("$anon"))), nme.CONSTRUCTOR), List())
                        ),
                        TermName("lazyStuff")
                      )

                      //println("block:"+block)

                      /*val reif = reify(
                        new com.webtrends.libs.json.util.LazyHelper[Format, A] {
                          override lazy val lazyStuff: Format[A] = null
                        }
                      )
                      //println("RAW:"+showRaw(reif.tree, printKinds = true))*/
                      c.Expr[Format[A]](block)
                    }
                  case l => c.abort(c.enclosingPosition, s"No implicit format for ${l.mkString(", ")} available.")
                }

              case None => c.abort(c.enclosingPosition, "No apply function found matching unapply parameters")
            }

        }
    }
  }

}
