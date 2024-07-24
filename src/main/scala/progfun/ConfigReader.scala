package progfun
import better.files._

final case class Config(
    name: String,
    inputPath: String,
    jsonPath: String,
    csvPath: String,
    yamlPath: String
)

object ConfigReader {
  def readConfig(filePath: String): Config = {
    val file = File(filePath)
    val jsonString = file.contentAsString
    val json = ujson.read(jsonString)

    val name = json("name").str
    val inputPath = json("inputPath").str
    val jsonPath = json("jsonPath").str
    val csvPath = json("csvPath").str
    val yamlPath = json("yamlPath").str

    Config(name, inputPath, jsonPath, csvPath, yamlPath)
  }
}
