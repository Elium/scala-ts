package com.mpc.scalats.core

object TypeScriptModel {

  sealed trait Declaration

  sealed trait TypeRef

  sealed trait AccessModifier

  case class CustomTypeRef(name: String, typeArgs: List[TypeRef]) extends TypeRef

  case class ArrayRef(innerType: TypeRef) extends TypeRef

  case class InterfaceDeclaration(name: String, members: List[Member], typeParams: List[String], parent: Option[String] = None) extends Declaration

  case class Member(name: String, typeRef: TypeRef)

  case class ClassConstructor(parameters: List[ClassConstructorParameter])

  case class ClassConstructorParameter(name: String,
                                       typeRef: TypeRef,
                                       accessModifier: Option[AccessModifier])

  case class UnknownTypeRef(name: String) extends TypeRef

  case object NumberRef extends TypeRef

  case object StringRef extends TypeRef

  case object BooleanRef extends TypeRef

  case object DateRef extends TypeRef

  case object DateTimeRef extends TypeRef

  case object NullRef extends TypeRef

  case object UndefinedRef extends TypeRef

  case class TypeParamRef(name: String) extends TypeRef

  case class UnionType(inners: TypeRef*) extends TypeRef

  case object AccessModifier {

    case object Public extends AccessModifier

    case object Private extends AccessModifier

  }

}