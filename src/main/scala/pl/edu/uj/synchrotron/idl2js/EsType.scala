package pl.edu.uj.synchrotron.idl2js

import org.apache.axis2.corba.idl.{types => idl}

package object EsType {

    def typename(d: idl.DataType): String = d match {
        case d: idl.PrimitiveDataType => d.getTypeName
        case d: idl.Typedef => d.getName
        case d: idl.EnumType => d.getName
        case d: idl.AbstractCollectionType => typename(d.getDataType) + "[]"
    }

    implicit class EsTypedef(t: idl.Typedef) {
        def toJs = {
//            val defn = "some-std-tpe"
            val defn = t.getDataType match {
                case d: idl.CompositeDataType => d.getName
                case d: idl.PrimitiveDataType => d.getTypeName
            }
            s"""/**
               | * @typedef ${defn} ${t.getName}
               | * @public
               | */""".stripMargin
        }
    }
}
