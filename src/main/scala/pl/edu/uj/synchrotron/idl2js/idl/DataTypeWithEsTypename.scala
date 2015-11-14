package pl.edu.uj.synchrotron.idl2js.idl

import org.apache.axis2.corba.idl.types._

package object DataTypeWithEsTypename {

    /** Wraps complex names in parentheses. */
    def sanitizeName(name: String) =
        if (name.exists { "|[]{}()".contains(_) }) s"($name)" else name

    implicit class DataTypeWithEsdocTypename(d: DataType) {

        def esTypename: String = d match {

            case d: PrimitiveDataType => d.getTypeName match {
                case ("int" | "long" | "short" | "byte") => "number"
                case ("double" | "float")                => "number"
                case ("boolean")                         => "boolean"
                case ("java.lang.String")                => "string"
                case ("void")                            => "undefined"
                case ("null")                            => "null"
                case _                                   => "Object"
            }

            case d: AbstractCollectionType =>
                s"${sanitizeName(d.getDataType.esTypename)}[]"
            case d: ConstType => d.getDataType.esTypename
            case d: EnumType => d.getName
            case d: Interface => d.getName
            case d: Struct => d.getName
            case d: Typedef => d.getDataType.esTypename
            case d: UnionType => d.getName
            case d: ValueType => d.getName
            case _ => "(UNKNOWN_TYPE)"
        }
    }
}
