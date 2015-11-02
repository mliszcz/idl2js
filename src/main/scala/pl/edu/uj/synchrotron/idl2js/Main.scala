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

    def main(args: Array[String]) = allCatch.either {

        val bytes = Files readAllBytes Paths.get(args.head)

        val sanitized = new String(bytes)
            .replaceAll("\\*\\*/\n", "*/\n")
            .replaceAll("readonly ", "")
            .getBytes(StandardCharsets.UTF_8)

        new ByteArrayInputStream(sanitized)

    }
    .right.flatMap(parseIDL)
    .right.map(generateECMAScript)
    .fold(println, println)

    def parseIDL(is: InputStream) = allCatch.either {

        val parser = new IDLParser(new IDLLexer(is))
        parser.specification

        val visitor = new IDLVisitor
        visitor.visit(parser.getAST)

        visitor.getIDL
    }

    def generateECMAScript(idl: IDL) = {

        println("interfaces:")
        for ((k,v) <- idl.getInterfaces) println(s"${k}: $v")

        println("composite types:")
        for ((k,v) <- idl.getCompositeDataTypes) println(s"${k}: $v")

        "done"
    }
}
