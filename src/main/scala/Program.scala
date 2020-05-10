import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.stream.Collectors

import cats.{Applicative, Id, Monad}
import cats.syntax.all._
import cats.instances.list._

import scala.collection.JavaConverters.asScalaBuffer
import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait GetFiles[F[_], Dir, File] {
  def getFiles(dir: Dir): F[List[File]]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

trait GetFileName[F[_], File] {
  def getFileName(file: File): F[String]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(file: File, dir: Dir): F[File]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               getFiles: GetFiles[F, Dir, File],
                               getFileName: GetFileName[F, File],
                               moveFile: MoveFile[F, Dir, File],
                               printer: Printer[F, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")
    files <- getFiles.getFiles(testDir)
    _ <- files.traverse(file => printer.printName(file))
    names <- files.traverse(file => getFileName.getFileName(file))
    firstLetters = names.map(name => name.head)
    newDirs <- firstLetters.traverse(letter => mkDir.mkDir(testDir, letter.toString))
    _ <- files.zip(newDirs).traverse(pair => moveFile.moveFile(pair._1, pair._2))
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path] with MkFile[F, Path, Path] with GetFiles[F, Path, Path] with GetFileName[F, Path] with MoveFile[F, Path, Path] {
  override def mkDir(dir: Path, name: String): F[Path] = {
    val resolved = dir.resolve(name)
    if (Files.exists(resolved)) {
      resolved.pure[F]
    } else {
      Files.createDirectories(resolved).pure[F]
    }
  }

  override def mkFile(dir: Path, name: String): F[Path] = {
    val resolved = dir.resolve(name)
    if (Files.exists(resolved) && Files.isRegularFile(resolved)) {
      resolved.pure[F]
    } else {
      Files.createFile(resolved).pure[F]
    }
  }

  override def getFiles(dir: Path): F[List[Path]] =
    asScalaBuffer(Files.walk(dir).filter(Files.isRegularFile(_)).collect(Collectors.toList[Path])).toList.pure[F]

  override def getFileName(file: Path): F[String] = file.getName(file.getNameCount - 1).toString.pure[F]

  override def moveFile(file: Path, dir: Path): F[Path] =  Files.move(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName.toString).pure[F]
}

object TypeClasses {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]
    program.run(Paths.get("."))
  }
}



