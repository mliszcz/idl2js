package pl.edu.uj.synchrotron.idl2js

import org.apache.axis2.corba.idl.types._

object DataTypeWithDependencyCheck {

    private def operationsDependOn(ops: Seq[Operation], d2: DataType) =
        ops.exists( op => {
            val args = op.getParams.toArray.toSeq.asInstanceOf[Seq[Member]]
            op.getReturnType.dependsOn(d2) || membersDependOn(args, d2)
        })

    private def membersDependOn(m: Seq[Member], d2: DataType) =
        m.exists { _.getDataType.dependsOn(d2) }

    implicit class DataTypeWithDependencyCheck(d1: DataType) {

        def dependsOn(d2: DataType): Boolean =
            // FIXME deal with circular dependencies
            // TODO check members for every composite types
            false
//                d1 match {
//                case _: PrimitiveDataType => false
//                case d: AbstractCollectionType => d.getDataType.dependsOn(d2)
//                case d: ConstType => d.getDataType.dependsOn(d2)
//                case _: EnumType => false
//                case d: Interface => operationsDependOn(d.getOperations, d2)
//                case d: Struct => d.getMembers.exists { _.getDataType.dependsOn(d2) }
//                case d: Typedef => d.getDataType.dependsOn(d2)
//                case d: UnionType => d.getMembers.exists { _.getDataType.dependsOn(d2) }
//                case d: ValueType => operationsDependOn(d.getOperations.values.asInstanceOf[Seq[Operation]], d2)
//                case _ => false
//            }

        def getPriority() = d1 match {
            case _: PrimitiveDataType => 0
            case _: Typedef => 1
            case _: ConstType => 2
            case _: EnumType => 3
            case _: UnionType => 4
            case _: Struct => 5
            case _: Interface => 6
            case _: AbstractCollectionType => 7
            case _: ValueType => 8
            case _ => 9
        }
    }
}
