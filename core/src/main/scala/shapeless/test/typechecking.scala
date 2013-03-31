/*
 * Copyright (c) 2013 Miles Sabin 
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless.test

import java.util.regex.Pattern
import java.io.{ PrintWriter, StringWriter }

import scala.io.Source
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter._

/**
 * A utility which ensures that a code fragment does not typecheck.
 * 
 * Credit: Jorge Ortiz (@JorgeO)
 */
class IllTyper[T](failure: String => T, success: String => T) {
  private val (interpreter, errorBuffer) = {
    val loader = manifest[shapeless.HNil].erasure.getClassLoader

    val settings = new Settings
    settings.classpath.value = Source.fromURL(loader.getResource("app.class.path")).mkString
    settings.bootclasspath.append(Source.fromURL(loader.getResource("boot.class.path")).mkString)
    settings.deprecation.value = true // enable detailed deprecation warnings
    settings.unchecked.value = true // enable detailed unchecked warnings
    
    val eb =  new StringWriter()
    
    val i = new IMain(settings, new PrintWriter(eb))
    i.interpret("""import shapeless._""")
    (i, eb)
  }

  def apply(code: String, expected: String = null): T = {
    val thunked = "() => { %s }".format(code)
    val expMsg = if(expected == null) "Expected some error." else "Expected error matching: "+expected

    errorBuffer.getBuffer.delete(0, errorBuffer.getBuffer.length)
    interpreter.interpret(thunked) match {
      case Results.Error =>
        val msg = errorBuffer.toString
        if(expected != null && !expected.matches(msg))
          failure("Type-checking failed in an unexpected way.\n"+expMsg+"\nActual error: "+msg)
        else
          success(msg)
      case Results.Success => failure("Type-checking succeeded unexpectedly.\n"+expMsg)
      case Results.Incomplete => throw new Exception("Incomplete code snippet")
    }
  }
}
