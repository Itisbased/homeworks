package hw

import java.nio.file.{Files, Path}
import scala.io.Source
import java.io.File

/**
 * Имплементируйте тесты самостоятельно.
 * Можно тестовые файлы положить в ресурсы, а можно создавать их на лету через `File.createTempFile`
 */
class StudentDatabaseSpec extends munit.FunSuite:

  private def createTempFile(content: String): Path = {
    val file = File.createTempFile("test", ".txt")
    Files.write(file.toPath, content.getBytes)
    file.toPath
  }

  test("StudentDatabase initialization should read data from init file"):
    val table1 = createTempFile("John,85\nAlice,90")
    val table2 = createTempFile("Bob,75\nCharlie,80")
    val initFile = createTempFile(s"table1|${table1}\ntable2|${table2}")

    val result = StudentDatabase.readFromFile(initFile.toString)
    assert(result.isRight)
    
    val db = result match {
      case Right(db) => db
      case Left(err) => fail(s"Expected Right, but got Left($err)")
    }
    assertEquals(db.getTable("table1").toOption.get, Vector(Student("John", 85), Student("Alice", 90)))
    assertEquals(db.getTable("table2").toOption.get, Vector(Student("Bob", 75), Student("Charlie", 80)))

    Files.delete(table1)
    Files.delete(table2)
    Files.delete(initFile)

  test("StudentDatabase initialization should fail if source file not exists"):
    val result = StudentDatabase.readFromFile("nonexistent.txt")
    assert(result.isLeft)

  test("StudentDatabase initialization should fail if some of link file not exists"):
    val table1 = createTempFile("John,85\nAlice,90")
    val initFile = createTempFile(s"table1|${table1}\ntable2|nonexistent.txt")

    val result = StudentDatabase.readFromFile(initFile.toString)
    assert(result.isLeft)
    assert(result.left.toOption.get.contains("nonexistent.txt"))

    Files.delete(table1)
    Files.delete(initFile)

  test("StudentDatabase initialization should fail if some of link format is invalid"):
    val initFile = createTempFile("table1|path1|extra\ninvalid_format")

    val result = StudentDatabase.readFromFile(initFile.toString)
    assert(result.isLeft)
    assert(result.left.toOption.get.contains("Invalid line format"))

    Files.delete(initFile)

  test("StudentDatabase should read existing table"):
    val table1 = createTempFile("John,85\nAlice,90")
    val initFile = createTempFile(s"table1|${table1}")

    val result = StudentDatabase.readFromFile(initFile.toString)
    assert(result.isRight)
    
    val db = result match {
      case Right(db) => db
      case Left(err) => fail(s"Expected Right, but got Left($err)")
    }
    assertEquals(db.getTable("table1").toOption.get, Vector(Student("John", 85), Student("Alice", 90)))

    Files.delete(table1)
    Files.delete(initFile)

  test("StudentDatabase reading table should fail if table not exists"):
    val table1 = createTempFile("John,85\nAlice,90")
    val initFile = createTempFile(s"table1|${table1}")

    val result = StudentDatabase.readFromFile(initFile.toString)
    assert(result.isRight)
    
    val db = result match {
      case Right(db) => db
      case Left(err) => fail(s"Expected Right, but got Left($err)")
    }
    assert(db.getTable("nonexistent").isLeft)

    Files.delete(table1)
    Files.delete(initFile)

  test("StudentDatabase reading table should fail if row format is invalid"):
    val table1 = createTempFile("John,85\nInvalidRow\nAlice,90")
    val initFile = createTempFile(s"table1|${table1}")

    val result = StudentDatabase.readFromFile(initFile.toString)
    assert(result.isRight)
    
    val db = result match {
      case Right(db) => db
      case Left(err) => fail(s"Expected Right, but got Left($err)")
    }
    assert(db.getTable("table1").isLeft)

    Files.delete(table1)
    Files.delete(initFile)

  test("StudentDatabase reading table should fail if grade format is invalid"):
    val table1 = createTempFile("John,invalid\nAlice,90")
    val initFile = createTempFile(s"table1|${table1}")

    val result = StudentDatabase.readFromFile(initFile.toString)
    assert(result.isRight)
    
    val db = result match {
      case Right(db) => db
      case Left(err) => fail(s"Expected Right, but got Left($err)")
    }
    assert(db.getTable("table1").isLeft)

    Files.delete(table1)
    Files.delete(initFile)

  test("StudentDatabase should provide proper information for determining best grade student via StudentStatisics"):
    val table1 = createTempFile("John,85\nAlice,90\nBob,75")
    val initFile = createTempFile(s"table1|${table1}")

    val result = StudentDatabase.readFromFile(initFile.toString)
    assert(result.isRight)
    
    val db = result match {
      case Right(db) => db
      case Left(err) => fail(s"Expected Right, but got Left($err)")
    }
    assertEquals(StudentStatisics.getBestGradeStudent(db, "table1").toOption.get, Some(Student("Alice", 90)))

    Files.delete(table1)
    Files.delete(initFile)

