package pl.edu.uj.synchrotron.idl2js

import org.apache.axis2.corba.idl.{types => idl}

package object EsType {

    private def sanitizeName(name: String) =
        if (name.exists { "|[]{}()".contains(_) }) s"($name)" else name

    implicit class DataTypeWithEsdocTypename(d: idl.DataType) {
        import idl._
        def esdocName: String = d match {
            case d: PrimitiveDataType => d.getTypeName match {
                case ("int" | "long" | "short" | "byte") => "Number"
                case ("double" | "float")                => "Number"
                case ("boolean")                         => "Boolean"
                case ("java.lang.String")                => "String"
                case _                                   => "Object"
            }
            case d: AbstractCollectionType =>
                s"${sanitizeName(d.getDataType.esdocName)}[]"
            case d: ConstType => d.getDataType.esdocName
            case d: EnumType => d.getName
            case d: Interface => d.getName
            case d: Struct => d.getName
            case d: Typedef => d.getDataType.esdocName
            case d: UnionType => d.getName
            case d: ValueType => d.getName // TODO what is 'ValueType'?
            case _ => "(UNKNOWN_TYPE)"
        }
    }

    implicit class EsTypedef(t: idl.Typedef) {
        def toJs =
            s"""/**
               | * @typedef {${t.getDataType.esdocName}} ${t.getName}
               | */""".stripMargin
    }

    implicit class EsEnum(t: idl.EnumType) {
        def toJs = {
            val mapping = t.getEnumMembers.toArray.toList
                .asInstanceOf[Seq[String]].zipWithIndex
                .map { case (key, idx) => s"$key: $idx" }
                .mkString(",\n|  ")
            s"""/**
               | * @type {Object}
               | * @public
               | */
               |export const ${t.getName} = Enum({
               |  $mapping
               |})""".stripMargin
        }
    }

    implicit class EsStruct(t: idl.Struct) {
        private def getter(m: idl.Member) =
            s"""|/** @type {${m.getDataType.esdocName}} */
                |get ${m.getName}() {
                |  return this._data.${m.getName}
                |}""".replace("|", "|  ")
        private def constructor(m: Seq[idl.Member]) =
            s"""|/** @param {Object} data */
                |constructor(data = {}) {
                |  /** @private */
                |  this._data = Object.assign({}, data)
                |}""".replace("|", "|  ")
        def toJs = {
            val attributes = t.getMembers.map(getter).mkString("\n\n")
            s"""/**
               | * @public
               | */
               |export class ${t.getName} {
               |
               ${constructor(t.getMembers)}
               |
               $attributes
               |}""".stripMargin
        }
    }

    implicit class EsUnion(t: idl.UnionType) {
        def toJs = {
            val members = t.getMembers.map(_.getDataType.esdocName)
                .map(sanitizeName).distinct.mkString("|")
            s"""/**
               | * @typedef {${members}} ${t.getName}
               | */""".stripMargin
        }
    }

    implicit class EsInterface(t: idl.Interface) {
        private def method(m: idl.Operation) = {
            val rawm = m.getParams.toArray.toSeq.asInstanceOf[Seq[idl.Member]]
            val mems = rawm.filterNot { _.getName equals ":" }
            val args = mems.map(_.getName).mkString(", ")
            val pars = mems.map(m => (m.getDataType.esdocName, m.getName))
                .map {
                    case (tpe, n) => s"| * @param {$tpe} $n $n"
                }.mkString("\n")
            s"""|/**
                | * @return {${m.getReturnType.esdocName}}
                ${if (pars.equals("")) "| *" else pars}
                | */
                |${m.getName}($args) { }""".replace("|", "|  ")
        }
        def toJs = {
            val methods = t.getOperations.filterNot { _.getName equals ":"
                } map { method } mkString("\n\n")
            val extds = t.getOperations.find { _.getName equals ":" } map {
                m => s"extends ${m.getReturnType.esdocName} "
            } getOrElse ""
            s"""/**
               | * @interface
               | * @public
               | */
               |export class ${t.getName} $extds{
               |
               |  constructor { }
               |
               $methods
               |}""".stripMargin
        }
    }
}
