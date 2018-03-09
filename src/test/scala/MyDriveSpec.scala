import org.scalatest._
import com.google.api.services.drive.model.{File => GFile}
import com.google.gson.GsonBuilder
import com.google.gson.reflect.TypeToken

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.io.Source

import MyDriveSpec._
import MyDriveService._


object MyDriveSpec {
  private val gson = new GsonBuilder()
    .registerTypeHierarchyAdapter(classOf[GFile], new GsonGFileAdapter())
    .registerTypeHierarchyAdapter(classOf[MyDir], new GsonMyDirAdapter())
    .create()
  
  private val totalFileListJson = Source.fromResource("totalfilelist.txt").getLines().toList.head
  private val rootDirGFileJson = Source.fromResource("rootdirgfile.txt").getLines().toList.head
  private val rootMyDirJson = Source.fromResource("rootmydir.txt").getLines().toList.head
  
  private val gfileJListType = new TypeToken[java.util.List[GFile]](){}.getType
  private val gfileType = new TypeToken[GFile](){}.getType
  private val myDirType = new TypeToken[MyDir](){}.getType
  
  private val totalFileJList: java.util.List[GFile] = gson.fromJson(totalFileListJson, gfileJListType)
  private val totalFileList = totalFileJList.asScala
  private val rootDirGFile: GFile = gson.fromJson(rootDirGFileJson, gfileType)
  private val rootMyDir: MyDir = gson.fromJson(rootMyDirJson, myDirType)
  private var curMyDir: MyDir = rootMyDir
  private val myDirStack = new MyDirStack()
}

class MyDriveSpec extends FlatSpec with Matchers {
  "verifyPath" should "return the root dir when given /" in {
    val result = verifyPath(rootMyDir, curMyDir, myDirStack, "/")
    assert(result.isLeft && result.left.get.equals(new EmptyFollowingPathFailure(rootMyDir)))
  }

  it should "return DoubleDotPlacementFailure when given /cmsc422_notes/../cmsc433_notes" in {
    val result = verifyPath(rootMyDir, curMyDir, myDirStack, "/cmsc422_notes/../cmsc433_notes")
    assert(result.isLeft && result.left.get.isInstanceOf[DoubleDotPlacementFailure])
  }

  it should "return DoubleDotPlacementFailure when given .../.." in {
    val result = verifyPath(rootMyDir, curMyDir, myDirStack, ".../..")
    assert(result.isLeft && result.left.get.isInstanceOf[DoubleDotPlacementFailure])
  }

  it should "return DuplicateNameFailure when given /DoubleTest/DTest/Blah" in {
    val result = verifyPath(rootMyDir, curMyDir, myDirStack, "/DoubleTest/DTest/Blah")
    assert(result.isLeft && result.left.get.equals(DuplicateNameFailure("DTest")))
  }

  it should "return DoubleDotCountFailure when given ../../../.." in {
    val result = verifyPath(rootMyDir, curMyDir, myDirStack, "../../../..")
    assert(result.isLeft && result.left.get.isInstanceOf[DoubleDotCountFailure])
  }

  it should "return InvalidPathFailure when given /ARTH200 Notes/Dropbox/HelloWorld/Dillinger" in {
    val result = verifyPath(rootMyDir, curMyDir, myDirStack, "/ARTH200 Notes/Dropbox/HelloWorld/Dillinger")
    assert(result.isLeft && result.left.get.equals(InvalidPathFailure("HelloWorld")))
  }

  it should "return a tuple when given whitespace" in {
    val result = verifyPath(rootMyDir, curMyDir, myDirStack, "   ")
    assert(result.isRight && result.right.get.equals(("   ", rootMyDir, List(), false)))
  }

  it should "return the root dir when changing directory to /ARTH200 Notes/Dropbox and given ../.." in {
    val arthMyDir = verifyPath(rootMyDir, curMyDir, myDirStack, "/ARTH200 Notes/Dropbox/").right.get._2
    val dropboxMyDir = verifyPath(rootMyDir, curMyDir, myDirStack, "/ARTH200 Notes/Dropbox/Dillinger").right.get._2
    myDirStack.push(arthMyDir)
    myDirStack.push(dropboxMyDir)

    val result = verifyPath(rootMyDir, dropboxMyDir, myDirStack, "../..")
  }
}

