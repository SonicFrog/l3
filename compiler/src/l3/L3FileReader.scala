package l3

import java.io.{ BufferedReader, InputStreamReader, FileInputStream }
import java.nio.file.Path
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable.ArrayBuilder

/**
 * File reading for L₃ (both modules and source files).
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object L3FileReader {
  def expandModules(base: Path, pathNames: Seq[String]): Seq[Path] = {
    def readModule(modulePath: Path): Seq[String] = {
      val moduleReader = newUTF8BufferedFileReader(modulePath)
      val moduleContents = (
        Iterator.continually(moduleReader.readLine)
          .takeWhile (_ != null)
          .map (_.trim)
          .filterNot { s => (s startsWith ";") || s.isEmpty })
        .toList
      moduleReader.close()
      moduleContents
    }

    def expand(base: Path, pathNames: Seq[String]): Seq[Path] = {
      val basePath = base.toAbsolutePath.normalize
      pathNames flatMap { pn =>
        val p = basePath.resolve(pn).normalize
        if (p.getFileName.toString endsWith ".ml3")
          expandModules(p.getParent, readModule(p))
        else
          Seq(p)
      }
    }
    expand(base, pathNames).distinct
  }

  def readFiles(basePath: Path, paths: Seq[Path]): (String, Int=>Position) = {
    def indexToPosition(indices: Array[Int],
                        fileLines: Array[(String, Int)])
                       (index: Int): Position = {
      val p = {
        val p0 = java.util.Arrays.binarySearch(indices, index)
        // FIXME: use code-points count to get column number, not char count!
        if (p0 < 0) (-p0 - 2) else p0
      }
      val (file, line) = fileLines(p)
      new Position(file, line, index - indices(p))
    }

    val progB = new StringBuilder()
    val indicesB: ArrayBuilder[Int] = ArrayBuilder.make()
    val fileLinesB: ArrayBuilder[(String, Int)] = ArrayBuilder.make()
    for (path <- paths) {
      val relPath = basePath relativize path
      val fileReader = newUTF8BufferedFileReader(path)
      Iterator.continually(fileReader.readLine)
        .takeWhile(_ != null)
        .zipWithIndex
        .foreach { case (line, lineIndex) =>
          {
            val index = progB.size
            progB ++= line; progB += '\n'
            indicesB += index
            fileLinesB += ((relPath.toString, lineIndex + 1))
          }
      }
      fileReader.close()
    }
    (progB.result, indexToPosition(indicesB.result, fileLinesB.result))
  }

  private def newUTF8BufferedFileReader(path: Path): BufferedReader =
    new BufferedReader(new InputStreamReader(new FileInputStream(path.toFile),
                                             UTF_8))
}
