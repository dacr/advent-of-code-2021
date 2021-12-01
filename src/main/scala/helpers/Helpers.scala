package helpers

import zio.*
import java.nio.file.{Path, Files}
import java.nio.charset.Charset

object Helpers {

  def readResourceContent(resourceName: String, charsetName: String = "UTF-8") = {
    for { // KO to enhance
      inputStream <- IO.attempt(Helpers.getClass.getResourceAsStream(resourceName))
      bytes       <- IO.attemptBlockingIO(inputStream.readAllBytes())
      content     <- IO.attempt(new String(bytes, charsetName))
    } yield content
  }

  def readPathContent(inputPath: Path, charsetName: String = "UTF-8") = for {
    charset <- IO.attempt(Charset.forName(charsetName))
    content <- IO.attemptBlockingIO(Files.readString(inputPath, charset))
  } yield content

  def readFileContent(filename: String, charsetName: String = "UTF-8") = for {
    inputPath <- IO.attempt(Path.of(filename))
    content   <- readPathContent(inputPath, charsetName)
  } yield content

  def writePathContent(outputPath: Path, content: String, charsetName: String = "UTF-8") = for {
    charset <- IO.attempt(Charset.forName(charsetName))
    _       <- IO.attemptBlockingIO(Files.writeString(outputPath, content, charset))
  } yield ()

  def writeFileContent(filename: String, content: String, charsetName: String = "UTF-8") = for {
    outputPath <- IO.attempt(Path.of(filename))
    _          <- writePathContent(outputPath, content, charsetName)
  } yield ()
}
