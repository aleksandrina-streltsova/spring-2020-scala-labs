import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.stream.Collectors

import cats.{Applicative, Id, Monad}
import cats.syntax.all._

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
  def printNames(files: List[File]): F[Unit]
}

trait SortFiles[F[_], Dir, File] {
  def sortFiles(dir: Dir, files: List[File]): F[Unit]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               getFiles: GetFiles[F, Dir, File],
                               sortFiles: SortFiles[F, Dir, File],
                               printer: Printer[F, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")
    files <- getFiles.getFiles(testDir)
    _ <- sortFiles.sortFiles(testDir, files)
    _ <- printer.printNames(files)
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path] with MkFile[F, Path, Path] with GetFiles[F, Path, Path] with SortFiles[F, Path, Path] {
  override def mkDir(dir: Path, name: String): F[Path] =
    Files.createDirectories(dir.resolve(name)).pure[F]

  override def mkFile(dir: Path, name: String): F[Path] =
    Files.createFile(dir.resolve(name)).pure[F]

  override def getFiles(dir: Path): F[List[Path]] =
    asScalaBuffer(Files.walk(dir).filter(Files.isRegularFile(_)).collect(Collectors.toList[Path])).toList.pure[F]

  private def getFileName(file: Path): String = file.getName(file.getNameCount - 1).toString

  private def getFirstLetter(file: Path): String = getFileName(file).substring(0, 1)

  override def sortFiles(dir: Path, files: List[Path]): F[Unit] = {
    val firstLetters = files.map(getFirstLetter).distinct
    firstLetters.foreach(letter => mkDir(dir, letter))
    var dirMap = Map[String, Path]()
    firstLetters.foreach(letter => dirMap += (letter -> Paths.get(dir.toString, letter)))
    files.foreach(file =>
      Files.move(file, dirMap(getFirstLetter(file)).resolve(getFileName(file)), StandardCopyOption.REPLACE_EXISTING))
  }.pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName.toString).pure[F]

  override def printNames(files: List[Path]): F[Unit] = println(files.map(_.getFileName.toString).mkString(", ")).pure[F]
}

object TypeClasse {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]
    program.run(Paths.get("."))
  }
}



