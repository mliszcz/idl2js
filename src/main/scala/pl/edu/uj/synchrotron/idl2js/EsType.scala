package pl.edu.uj.synchrotron.idl2js

import org.apache.axis2.corba.idl.{types => idl}

package object EsType {

    implicit class DataTypeWithEsdocTypename(d: idl.DataType) {
        import idl._
        def esdocName: String = d match {
            case d: PrimitiveDataType => d.getTypeName match {
                case ("int" | "long" | "short" | "byte") => "Number"
                case ("double" | "float") => "Number"
                case ("boolean") => "Boolean"
                case ("java.lang.String") => "String"
                case _ => "Object"
            }
            case d: AbstractCollectionType =>
                val name = d.getDataType.esdocName
                if (name.exists {
                    Seq('[',']','|','{','}','(',')').contains(_)
                }) s"($name)[]" else s"$name[]"
            case d: ConstType => d.getDataType.esdocName
            case d: EnumType => d.getName
            case d: Interface => d.getName
            case d: Struct => d.getName // TODO handle as `record`
            case d: Typedef => d.getDataType.esdocName
            case d: UnionType =>
                val members = d.getMembers.map(_.getDataType.esdocName).mkString("|")
                s"{$members}"
            case d: ValueType => d.getName
            case _ => "(UNKNOWN_TYPE)"
        }
    }

    implicit class EsTypedef(t: idl.Typedef) {
        def toJs =
            s"""/**
               | * @typedef {${t.getDataType.esdocName}} ${t.getName}
               | * @public
               | */""".stripMargin
    }

    implicit class EsEnum(t: idl.EnumType) {
        def toJs = {
            val mapping = t.getEnumMembers.toArray.toList
                .asInstanceOf[Seq[String]].zipWithIndex
            val listing = mapping.map { case (key, idx) =>
                s"$key: $idx" } mkString(",\n|  ")
            s"""/**
               | * @type {Object}
               | * @public
               | */
               |${t.getName} = Enum({
               |  $listing
               |})""".stripMargin
        }
    }
}
