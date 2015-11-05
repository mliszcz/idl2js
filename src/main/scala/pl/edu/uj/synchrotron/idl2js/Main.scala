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
        import DataTypeWithDependencyCheck._

        idl.getCompositeDataTypes.toSeq.sortWith {
            case ( (_, t1: DataType), (_, t2: DataType) ) =>
                if (t1.getClass equals t2.getClass) (t1, t2) match {
                    case (t1: PrimitiveDataType, t2: PrimitiveDataType) =>
                        t1.getTypeName < t2.getTypeName
                    case (t1: CompositeDataType, t2: CompositeDataType) =>
                        t1.getName < t2.getName
                }
                else if (t1 dependsOn t2) true
                else t1.getPriority < t2.getPriority
        }.asInstanceOf[Seq[(String, CompositeDataType)]]
    }

    def generateECMAScript(types: Seq[(String, org.apache.axis2.corba.idl.types.CompositeDataType)]) = {

        import org.apache.axis2.corba.idl.types._

        import EsType._

//        implicit class TypedefWithToJs(d1: Typedef) {
//            def toJs = {
//                println("TYPEDEF")
//                "TYPEDEF"
//            }
//        }

        val jsTypes = types.map {
            case (_, v: Typedef) => v.toJs
            case (_, v) => s"""// UNKNOWN TYPE: ${v.getName}"""
        }

        jsTypes.mkString("\n\n")
    }

    def debugIDL(types: Seq[(String, org.apache.axis2.corba.idl.types.CompositeDataType)]) = {

        import org.apache.axis2.corba.idl.types._

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
        "done"
    }
}
