// Copied with permission from https://github.com/scala-js/scala-js

package scala.scalajs.js.annotation

import scala.annotation.Annotation

class JSExportAll extends scala.annotation.Annotation
class JSExportDescendentObjects extends scala.annotation.Annotation
class JSExportDescendentClasses extends scala.annotation.Annotation

class JSExportNamed extends scala.annotation.Annotation {
  def this(name: String) = this()
}

class JSExport extends scala.annotation.Annotation {
  def this(name: String) = this()
}
