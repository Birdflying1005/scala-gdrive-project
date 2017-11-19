import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.drive.{Drive, DriveScopes}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
//import util.Try

object MyDrive {
  private val APPLICATION_NAME = "scala-gdrive-project"
  private val DATA_STORE_DIR = System.getProperty("user.home") + "/.credentials/scala-gdrive-project"
  private val DATA_STORE_FACTORY =
    new FileDataStoreFactory(new java.io.File(DATA_STORE_DIR))
  private val JSON_FACTORY = JacksonFactory.getDefaultInstance
  private val HTTP_TRANSPORT = GoogleNetHttpTransport.newTrustedTransport
  private val SCOPES = ListBuffer[String](DriveScopes.DRIVE).asJava

  def authorize: Credential = {
    val clientSecrets = GoogleClientSecrets.load(JSON_FACTORY,
      new java.io.InputStreamReader(new java.io.FileInputStream(DATA_STORE_DIR + "/client_id.json")))
    val flow =
      new GoogleAuthorizationCodeFlow.Builder(HTTP_TRANSPORT, JSON_FACTORY, clientSecrets, SCOPES)
        .setDataStoreFactory(DATA_STORE_FACTORY)
        .setAccessType("offline")
        .build()
    new AuthorizationCodeInstalledApp(flow, new LocalServerReceiver).authorize("user")
  }

  def getDriveService: com.google.api.services.drive.Drive = {
    new Drive.Builder(
      HTTP_TRANSPORT, JSON_FACTORY, authorize)
      .setApplicationName(APPLICATION_NAME)
      .build()
  }

  def main(args: Array[String]): Unit = {
    val service = getDriveService
    val result = service.files.list
      .setPageSize(10)
      .setFields("nextPageToken, files(id, name)")
      .execute()
    var files = result.getFiles

    println(files.get(0).getClass)
    println("Files: ")
    files.forEach(file => {
      val name = file.getName
      val id = file.getId
      println(s"$name ($id)\n")
    })
  }
}
