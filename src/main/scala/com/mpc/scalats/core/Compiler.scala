package com.mpc.scalats.core

import com.mpc.scalats.configuration.Config
import com.mpc.scalats.core.TypeScriptModel.{
  ClassConstructor,
  ClassConstructorParameter,
  InterfaceDeclaration,
  NullRef,
  ParentInterfaceDeclaration,
  UndefinedRef,
  UnionType
}

/**
  * Created by Milosz on 09.06.2016.
  */
class Compiler(scalaClasses: List[ScalaModel.CaseClass]) {

  lazy val parentClasses = {
    val parentsNames =
      scalaClasses
        .filter(_.parent.nonEmpty)
        .groupBy(_.parent.get)
        .keys
        .toList
    scalaClasses.filter(sc => parentsNames.contains(sc.name))
  }

  lazy val childrenClasses = scalaClasses.diff(parentClasses)

  lazy val parentChildrenMap = {
    parentClasses
      .map(c => c.name -> scalaClasses.filter(_.parent.contains(c.name)))
      .toMap
  }

  def compile()(implicit config: Config): List[TypeScriptModel.Declaration] = {

    /* We emit full typings for children classes, and for "parent classes" (so, sealed traits)
    which have actual attributes. For attribute-less sealed traits, we emit only an union type.
    For attribute-full sealed traits, we emit both ! The union type is used for attributes reference, and the
    sealed trait type is used for class hierarchy. Yeah, front is weird :shrugh:
     */
    val meaningfulParentClasses = parentClasses.filter(_.members.nonEmpty)
    parentClasses.flatMap { parentClass =>
      List(compileParentInterface(parentClass))
    } ++ (childrenClasses ++ meaningfulParentClasses).flatMap { scalaClass =>
      List(compileInterface(scalaClass))
    }
  }

  def compileParentInterface(
    parentClass: ScalaModel.CaseClass
  )(implicit config: Config): ParentInterfaceDeclaration = {
    ParentInterfaceDeclaration(
      getEmittedName(parentClass),
      scalaClasses
        .filter(_.parent.contains(parentClass.name))
        .map(getEmittedName)
    )
  }

  private def compileInterface(
    scalaClass: ScalaModel.CaseClass
  )(implicit config: Config): InterfaceDeclaration = {
    TypeScriptModel.InterfaceDeclaration(
      getEmittedName(scalaClass),
      scalaClass.members map { scalaMember =>
        TypeScriptModel
          .Member(scalaMember.name, compileTypeRef(scalaMember.typeRef))
      },
      typeParams = scalaClass.params,
      parent = None
    )
  }

  private def getEmittedName(
    c: ScalaModel.CaseClass
  )(implicit config: Config) = {
    val prefix = config.interfacePrefix
    config.customNameMap
      .getOrElse(c.name, s"$prefix${c.name}")
  }

  def makeEmittedName(name: String, parentMap: Map[String, List[String]])(
    implicit config: Config
  ): String = {
    val unionNames = parentMap.getOrElse(name, List(name))

    config.customNameMap.getOrElse(name, s"${config.interfacePrefix}$name")
  }

  private def compileTypeRef(
    scalaTypeRef: ScalaModel.TypeRef
  )(implicit config: Config): TypeScriptModel.TypeRef = scalaTypeRef match {
    case ScalaModel.IntRef =>
      TypeScriptModel.NumberRef
    case ScalaModel.LongRef =>
      TypeScriptModel.NumberRef
    case ScalaModel.DoubleRef =>
      TypeScriptModel.NumberRef
    case ScalaModel.FloatRef =>
      TypeScriptModel.NumberRef
    case ScalaModel.BigDecimalRef =>
      TypeScriptModel.NumberRef
    case ScalaModel.BooleanRef =>
      TypeScriptModel.BooleanRef
    case ScalaModel.StringRef =>
      TypeScriptModel.StringRef
    case ScalaModel.SeqRef(innerType) =>
      TypeScriptModel.ArrayRef(compileTypeRef(innerType))
    case ScalaModel.CaseClassRef(name, typeArgs) => {
      config.customNameMap.get(name) match {
        case Some(customName) =>
          TypeScriptModel
            .CustomTypeRef(customName, typeArgs.map(compileTypeRef(_)))
        case None =>
          parentChildrenMap.get(name) match {
            case Some(children) =>
              UnionType(children.map(compileTypeRef): _*)
            case None =>
              TypeScriptModel
                .CustomTypeRef(
                  s"${config.interfacePrefix}$name",
                  typeArgs.map(compileTypeRef(_))
                )
          }
      }
    }
    case ScalaModel.DateRef =>
      TypeScriptModel.DateRef
    case ScalaModel.DateTimeRef =>
      TypeScriptModel.DateTimeRef
    case ScalaModel.TypeParamRef(name) =>
      TypeScriptModel.TypeParamRef(name)
    case ScalaModel.OptionRef(innerType)
        if config.optionToNullable && config.optionToUndefined =>
      TypeScriptModel
        .UnionType(compileTypeRef(innerType), NullRef, UndefinedRef)
    case ScalaModel.OptionRef(innerType) if config.optionToNullable =>
      TypeScriptModel
        .UnionType(compileTypeRef(innerType), NullRef)
    case ScalaModel.OptionRef(innerType) if config.optionToUndefined =>
      TypeScriptModel
        .UnionType(compileTypeRef(innerType), UndefinedRef)
    case ScalaModel.UnknownTypeRef(name) =>
      config.customNameMap
        .get(name)
        .map(TypeScriptModel.UnknownTypeRef)
        .getOrElse(
          TypeScriptModel.UnknownTypeRef(s"${config.interfacePrefix}$name")
        )
  }
}
