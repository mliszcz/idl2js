package pl.edu.uj.synchrotron.idl2js

import scala.collection.JavaConversions._
import org.apache.axis2.corba.idl.parser._
import org.apache.axis2.corba.idl.types.IDL
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.util.control.Exception._
import java.io.{InputStream, ByteArrayInputStream}

// http://www.programcreek.com/java-api-examples/index.php?api=antlr.collections.AST
// https://issues.apache.org/jira/browse/AXIS2-5328
// http://stackoverflow.com/questions/25967918/com-sun-corba-class-not-found-in-java-7-update-67

object Main {

    def main(args: Array[String]) =
        allCatch.either { args.head }
        .right.flatMap(loadIDL)
        .right.flatMap(parseIDL)
        .right.map(sortTypes)
        .right.map(generateECMAScript)
        .fold(println, println)

    def loadIDL(filename: String) = allCatch.either {

        val bytes = Files readAllBytes Paths.get(filename)

        val sanitized = new String(bytes)
            .replaceAll("\\*\\*/\n", "*/\n")
            .replaceAll("readonly ", "")
            .getBytes(StandardCharsets.UTF_8)

        new ByteArrayInputStream(sanitized)
    }

    def parseIDL(is: InputStream) = allCatch.either {

        val parser = new IDLParser(new IDLLexer(is))
        parser.specification

        val visitor = new IDLVisitor
        visitor.visit(parser.getAST)

        visitor.getIDL
    }

    def sortTypes(idl: IDL) = {

        import org.apache.axis2.corba.idl.types._

        def operationsDependOn(ops: Seq[Operation], d2: DataType) =
            ops.exists { op => (
                    op.getReturnType().dependsOn(d2)
                    || op.getParams.toArray.toSeq.asInstanceOf[Seq[Member]].exists {  _.getDataType.dependsOn(d2) }
            )}

        // TODO members check in each type ??

        implicit class DataTypeWithDependencyCheck(d1: DataType) {

            def dependsOn(d2: DataType): Boolean =
//                if (d1.getClass equals d2.getClass) (d1, d2) match {
//                case (t1: PrimitiveDataType, t2: PrimitiveDataType) => t1.getTypeName == t2.getTypeName
//                case (t1: CompositeDataType, t2: CompositeDataType) => t1.getName == t2.getName
//            } else
                d1 match {
                case _: PrimitiveDataType => false
                case d: AbstractCollectionType => d.getDataType.dependsOn(d2)
                case d: ConstType => d.getDataType.dependsOn(d2)
                case _: EnumType => false
                case d: Interface => operationsDependOn(d.getOperations, d2)
                case d: Struct => d.getMembers.exists { _.getDataType.dependsOn(d2) }
                case d: Typedef => d.getDataType.dependsOn(d2)
                case d: UnionType => d.getMembers.exists { _.getDataType.dependsOn(d2) }
                case d: ValueType => operationsDependOn(d.getOperations.values.asInstanceOf[Seq[Operation]], d2)
                case _ => false
            }

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

        idl.getCompositeDataTypes.toSeq.sortWith { case ( (_, t1: DataType), (_, t2: DataType) ) =>
            if (t1.getClass equals t2.getClass) (t1, t2) match {
                case (t1: PrimitiveDataType, t2: PrimitiveDataType) => t1.getTypeName < t2.getTypeName
                case (t1: CompositeDataType, t2: CompositeDataType) => t1.getName < t2.getName
            }
            else if (t1 dependsOn t2) false
            else t1.getPriority < t2.getPriority
        }.asInstanceOf[Seq[(String, CompositeDataType)]]

//        idl.getCompositeDataTypes.toSeq.sortWith { case ( (_, t1), (_, t2) ) =>
//            // is t1 < t2 ?
//            (t1, t2) match {
//
//                case (t1: Typedef, t2: Typedef) => t1.getName < t2.getName
//                case (t1: Typedef, _) => true
//
//                case (t1: EnumType, _: Typedef) => false
//                case (t1: EnumType, t2: EnumType) => t1.getName < t2.getName
//                case (t1: EnumType, _) => true
//
//                case (t1: UnionType, _: Typedef) => false
//                case (t1: UnionType, _: EnumType) => false
//                case (t1: UnionType, t2: UnionType) => t1.getName < t2.getName
//                case (t1: UnionType, _) => true
//
//                case (t1: Struct, t2: Typedef) => false
//                case (t1: Struct, t2: EnumType) => false
//                case (t1: Struct, t2: UnionType) => false
//                case (t1: Struct, t2: Struct) => t1.getName < t2.getName // TODO handle dependent types
//                case (t1: Struct, _) => true
//
//                case (t1: Interface, t2: Typedef) => false
//                case (t1: Interface, t2: EnumType) => false
//                case (t1: Interface, t2: UnionType) => false
//                case (t1: Interface, t2: Struct) => false
//                case (t1: Interface, t2: Interface) => t1.getName < t2.getName // TODO handle dependent types
//                case (t1: Interface, _) => true
//
//                case (t1: CompositeDataType, t2: CompositeDataType) => t1.getName < t2.getName
//            }
//        }.asInstanceOf[Seq[(String, CompositeDataType)]]
    }

    def generateECMAScript(types: Seq[(String, org.apache.axis2.corba.idl.types.CompositeDataType)]) = {

        import org.apache.axis2.corba.idl.types._
//        idl.getCompositeDataTypes.map { case (k, v) =>
        types.map { case (k, v) =>
            print(s"${v.getName}(${v.getClass.getSimpleName}): ")
            v match {
                case e: EnumType =>
                    println(s"${e.getEnumMembers}")
                case e: Typedef =>
                    println(s"${e.getDataType}")
                case e: ConstType =>
                    println(s"${e.getDataType}")
                case e: UnionType =>
                    println(s"${e.getDiscriminatorType}")
                case e: Struct =>
                    println(s"")
                case e: Interface =>
                    val ops = e.getOperations.map(op => {
                        val ret = op.getReturnType
                        val mem = op.getParams.toArray.toSeq.asInstanceOf[Seq[Member]]
                        val memstr = mem.map { _.getDataType }
                        s"[${ret} -> {${memstr.toList}}]"
                    }).toList
                    println(s"OPS: ${ops}")
                case e: AbstractCollectionType =>
                    println(s"${e.getDataType}")
                case e: ValueType =>
                    println(s"")
            }
            println(s"MEMBERS: [${v.getMembers.toSeq.map(_.getDataType)}]")
        }
// [${e.getMembers.toSeq.map(_.getDataType)}]
//        println("interfaces:")
//        for ((k,v) <- idl.getInterfaces) println(s"${k}: $v")
//
//        println("composite types:")
//        for ((k,v) <- idl.getCompositeDataTypes) println(s"${k}: $v")

        "done"
    }
}
