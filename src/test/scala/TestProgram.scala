import java.nio.file.{Files, Path, Paths}
import java.util.Comparator

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestProgram extends AnyFlatSpec with Matchers {
  val tmp: Path = Paths.get("./tmp")
  Files.createDirectory(tmp)
  implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
  val program = new Program[Id, Path, Path]
  program.run(tmp)

  val testDir: Path = tmp.resolve("test_dir")
  val fDir: Path = testDir.resolve("f")
  val bDir: Path = testDir.resolve("b")
  val foo: Path = fDir.resolve("foo")
  val bar: Path = bDir.resolve("bar")
  val baz: Path = bDir.resolve("baz")

  Files.exists(testDir) && Files.isDirectory(testDir) shouldBe true
  Files.exists(fDir) && Files.isDirectory(fDir) shouldBe true
  Files.exists(fDir) && Files.isDirectory(bDir) shouldBe true
  Files.exists(foo) && Files.isRegularFile(foo) shouldBe true
  Files.exists(bar) && Files.isRegularFile(bar) shouldBe true
  Files.exists(baz) && Files.isRegularFile(baz) shouldBe true

  Files.walk(tmp)
    .sorted(Comparator.reverseOrder())
    .forEach(file => Files.deleteIfExists(file))
}