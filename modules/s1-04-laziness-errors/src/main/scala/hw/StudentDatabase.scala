package hw

import java.nio.file.Path
import scala.annotation.nowarn
import scala.io.Source
import scala.util.{Try, Success, Failure}
import java.io.File

/** 1. База данных студентов оценками.
 *
 * Необходимо написать имплементацию аналога базы данных студентов.
 * База студентов состоит из разных таблиц студентов (их имен).
 * База должны вычитываться из загрузочного файла, в котором хранятся ссылки на таблицы.
 * По сути файл состоит из набора строк вида "<название_таблицы>|<путь до файла таблицы>".
 *
 * При первоначальной загрузке базы, необходимо парсить ссылки на таблицы и проверять, существует ли файл этой таблицы.
 * По итогу инициализации, если хотя бы одна ссылка не была распарсена или один файл таблицы из распарсенной ссылки не был найден,
 * то должна формироваться ошибка, в которой содержится информация обо всех таких подошибках.
 * При обращении к конкретному списку студентов через интерфейс StudentDatabase файл должен вычитываться заново.
 * Если таблицы с конректным именем не существует или произошла системная ошибка чтения файла таблицы, то должна формироваться ошибка.
 *
 * Смоделируйте ошибки, имплементируйте нереализованные методы и напишите тесты.
 */

type GetTableError = String

type DbInitError = String

case class Student(name: String, grade: Short)

trait StudentDatabase {

  /**
   * Возвращает содержимое таблицы студентов по имени таблицы.
   */
  def getTable(tableName: String): Either[GetTableError, Vector[Student]]
}

object StudentDatabase {
  @nowarn
  private final class Impl(tables: Map[String, Path]) extends StudentDatabase {
    def getTable(tableName: String): Either[GetTableError, Vector[Student]] = {
      tables.get(tableName) match {
        case None => Left(s"Table '$tableName' not found")
        case Some(path) =>
          if !path.toFile.exists() then Left(s"Table file not found: $path")
          else
            Try {
              val source = Source.fromFile(path.toFile)
              try {
                val students = source
                  .getLines()
                  .map { line =>
                    line.split(",") match {
                      case Array(name, grade) =>
                        Try(Student(name.trim, grade.trim.toShort)) match {
                          case Success(student) => Right(student)
                          case Failure(_)       => Left(s"Invalid grade format in line: $line")
                        }
                      case _ => Left(s"Invalid row format: $line")
                    }
                  }
                  .toVector

                if students.exists(_.isLeft) then Left(students.collect { case Left(error) => error }.mkString("\n"))
                else Right(students.collect { case Right(student) => student })
              } finally {
                source.close()
              }
            }.toEither.left.map(_.getMessage).flatMap(identity)
      }
    }
  }

  /**
   * Загружает базу данных из файла sourceFileName.
   */
  def readFromFile(sourceFileName: String): Either[DbInitError, StudentDatabase] = {
    if !new File(sourceFileName).exists() then Left(s"Source file not found: $sourceFileName")
    else
      Try {
        val source = Source.fromFile(sourceFileName)
        try {
          val tableEntries = source
            .getLines()
            .map { line =>
              line.split("\\|") match {
                case Array(name, path) => Right((name.trim, Path.of(path.trim)))
                case _                 => Left(s"Invalid line format: $line")
              }
            }
            .toVector

          if tableEntries.exists(_.isLeft) then Left(tableEntries.collect { case Left(error) => error }.mkString("\n"))
          else {
            val tables = tableEntries.collect { case Right((name, path)) => (name, path) }.toMap

            val nonExistentFiles = tables.values.filterNot(path => path.toFile.exists())
            if nonExistentFiles.nonEmpty then Left(nonExistentFiles.map(_.toString).mkString("\n"))
            else Right(new Impl(tables))
          }
        } finally {
          source.close()
        }
      }.toEither.left.map(_.getMessage).flatMap(identity)
  }
}

object StudentStatisics {

  /**
   * Возвращает студента с максимальным баллом из конкретной таблицы.
   */
  def getBestGradeStudent(
    db: StudentDatabase,
    tableName: String
  ): Either[GetTableError, Option[Student]] = {
    db.getTable(tableName).map { students =>
      if students.isEmpty then None
      else Some(students.maxBy(_.grade))
    }
  }
}
