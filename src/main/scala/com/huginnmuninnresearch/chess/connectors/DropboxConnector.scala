package com.huginnmuninnresearch.chess.connectors

import java.io.{FileInputStream, IOException}

import com.dropbox.core.v2.DbxClientV2
import com.dropbox.core.v2.common.PathRoot
import com.dropbox.core.{DbxException, DbxRequestConfig}
import com.huginnmuninnresearch.chess.connectors.DropboxConnector.DropboxCredentials
import net.liftweb.json.{DefaultFormats, _}

import scala.util.Using
import scala.util.control.Breaks.break

class DropboxConnector(credentials: DropboxCredentials) {

  val client: DbxClientV2 = linkAccount()

  @throws[DbxException]
  def linkAccount(): DbxClientV2 = {
    val config = DbxRequestConfig.newBuilder("dropbox").build
    val client = new DbxClientV2(config, credentials.accessToken)
    println(s"Connected to Account: ${client.users.getCurrentAccount.getName.getDisplayName}")
    client
  }

  @throws(classOf[IOException])
  @throws(classOf[DbxException])
  def fileLoader(dropboxFilePath: String): Unit = {

    client.withPathRoot(PathRoot.root(client.users().getCurrentAccount().getRootInfo().getRootNamespaceId()))
    var result = client.files().listFolder(dropboxFilePath)
    while (true) {
      for (metadata <- result.getEntries.toArray) {
        println(metadata)
      }

      if (!result.getHasMore) {
        break
      }
        result = client.files().listFolderContinue(result.getCursor)
    }

    Using(new FileInputStream("test.txt")) { in =>
      val metadata = client.files().uploadBuilder("/test.txt").uploadAndFinish(in)
    }
  }
}

object DropboxConnector {
  import scala.io.Source
  implicit val formats: DefaultFormats.type = DefaultFormats
  val pathToCredentials = "resources/dropbox_key.json"
  val pathToDropboxChessDatabase = "/Fun/Games/FireQuest Text-Based Adventure.docx"

  case class DropboxCredentials(key: String = "key", secret: String = "secret", accessToken: String = "accessToken")

  val credentials: DropboxCredentials = Using(Source.fromFile(pathToCredentials)) { jsonFile =>
    parse(jsonFile.getLines().mkString).extract[DropboxCredentials]
  }.getOrElse(DropboxCredentials())

  def main(args: Array[String]): Unit = {
    val connector = new DropboxConnector(credentials)
    connector.fileLoader(pathToDropboxChessDatabase)
  }
}
