package helpers

import zio.*
import java.nio.file.{Path, Files}
import java.nio.charset.Charset

object Helpers {

  def readPathContent(inputPath: Path) = for {
    charset <- IO.attempt(Charset.forName("UTF-8"))
    content <- IO.attempt(Files.readString(inputPath, charset))
  } yield content

  def readFileContent(filename: String) = for {
    inputPath <- IO.attempt(Path.of(filename))
    content   <- readPathContent(inputPath)
  } yield content

  def writePathContent(outputPath: Path, content: String) = for {
    charset <- IO.attempt(Charset.forName("UTF-8"))
    _       <- IO.attempt(Files.writeString(outputPath, content, charset))
  } yield ()
}
