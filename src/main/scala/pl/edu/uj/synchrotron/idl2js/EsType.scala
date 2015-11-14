package pl.edu.uj.synchrotron.idl2js

import org.apache.axis2.corba.idl.{types => axisidl}
import idl.DataTypeWithEsTypename._
import scala.language.implicitConversions

package object EsType {

    implicit class EsTypedef(t: axisidl.Typedef) {
        def toJs =
            s"""/**
               | * @typedef {${t.getDataType.esTypename}} ${t.getName}
               | */""".stripMargin
    }

    implicit class EsEnum(t: axisidl.EnumType) {
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

    implicit class EsStruct(t: axisidl.Struct) {
        private def getter(m: axisidl.Member) =
            s"""|/** @type {${m.getDataType.esTypename}} */
                |get ${m.getName}() {
                |  return this._data.${m.getName}
                |}""".replace("|", "|  ")
        private def constructor(m: Seq[axisidl.Member]) =
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

    implicit class EsUnion(t: axisidl.UnionType) {
        def toJs = {
            val members = t.getMembers.map(_.getDataType.esTypename)
                .map(sanitizeName).distinct.mkString("|")
            s"""/**
               | * @typedef {${members}} ${t.getName}
               | */""".stripMargin
        }
    }

    implicit class EsInterface(t: axisidl.Interface) {
        private def method(m: axisidl.Operation) = {
            val rawm = m.getParams.toArray.toSeq.asInstanceOf[Seq[axisidl.Member]]
            val mems = rawm.filterNot { _.getName equals ":" }
            val args = mems.map(_.getName).mkString(", ")
            val pars = mems.map(m => (m.getDataType.esTypename, m.getName))
                .map {
                    case (tpe, n) => s"| * @param {$tpe} $n $n"
                }.mkString("\n")
            s"""|/**
                | * @return {${m.getReturnType.esTypename}}
                ${if (pars.equals("")) "| *" else pars}
                | */
                |${m.getName}($args) { }""".replace("|", "|  ")
        }
        def toJs = {
            val methods = t.getOperations.filterNot { _.getName equals ":"
                } map { method } mkString("\n\n")
            val extds = t.getOperations.find { _.getName equals ":" } map {
                m => s"extends ${m.getReturnType.esTypename} "
            } getOrElse ""
            s"""/**
               | * @interface
               | * @public
               | */
               |export class ${t.getName} $extds{
               |
               |  constructor() { }
               |
               $methods
               |}""".stripMargin
        }
    }
}
