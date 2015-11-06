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

        val jsTypes = types.map {
            case (_, v: Typedef) => v.toJs
            case (_, v: EnumType) => v.toJs
            case (_, v: Struct) => v.toJs
            case (_, v: UnionType) => v.toJs
            case (_, v: Interface) => v.toJs
            case (_, v) => s"""// UNKNOWN TYPE: ${v.getName}"""
        }

        jsTypes.mkString("\n\n")
    }
}
