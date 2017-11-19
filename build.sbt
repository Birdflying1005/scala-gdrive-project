name := "scala-gdrive-project"

version := "0.1"

scalaVersion := "2.12.4"
        
libraryDependencies ++= List(
  "com.google.api-client" % "google-api-client" % "1.23.0",
  "com.google.apis" % "google-api-services-drive" % "v3-rev90-1.23.0",
  "com.google.api-client" % "google-api-client-java6" % "1.23.0",
  "com.google.oauth-client" % "google-oauth-client" % "1.23.0",
  "com.google.oauth-client" % "google-oauth-client-jetty" % "1.23.0",
  "com.google.apis" % "google-api-services-oauth2" % "v2-rev131-1.23.0",
  "com.google.api.client" % "google-api-client-javanet" % "1.2.3-alpha",
  "com.google.http-client" % "google-http-client" % "1.23.0",
  "com.google.http-client" % "google-http-client-jackson2" % "1.23.0",
  "com.google.api.client" % "google-api-client-json" % "1.2.3-alpha",
  "com.google.api.client" % "google-api-client-util" % "1.2.3-alpha",
  "org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)
