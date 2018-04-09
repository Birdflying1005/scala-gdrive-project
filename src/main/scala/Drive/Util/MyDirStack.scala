package Drive.Util

class MyDirStack {
  private var stack = List[MyDir]()

  def isEmpty: Boolean = {
    stack.isEmpty
  }

  def clear: Unit = {
    stack = List[MyDir]()
  }

  def pop: MyDir = {
    val result = stack.head
    stack = stack.tail
    result
  }

  def push(dir: MyDir): Unit = {
    stack = dir +: stack
  }

  def peek: MyDir = {
    stack.head
  }

  def getBackedList: List[MyDir] = {
    stack
  }

  def length = {
    stack.length
  }
}
