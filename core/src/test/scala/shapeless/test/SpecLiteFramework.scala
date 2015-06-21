// Based on work from Rickard Nilsson at http://www.scalacheck.org

package shapeless.test

import java.util.concurrent.atomic.AtomicInteger
import scala.Console.{GREEN, RED, RESET, BLUE}
import org.scalajs.testinterface.TestUtils.{loadModule, newInstance}
import sbt.testing._

import scala.language.reflectiveCalls

private abstract class SpecLiteRunner(
  val args: Array[String],
  val remoteArgs: Array[String],
  val loader: ClassLoader
) extends Runner {

  val successCount = new AtomicInteger(0)
  val failureCount = new AtomicInteger(0)
  val errorCount = new AtomicInteger(0)
  val testCount = new AtomicInteger(0)

  val params = SpecLiteTest.cmdLineParser.parseParams(args) match {
    case Some(p) => p.withTestCallback(new SpecLiteTest.TestCallback {})
    case None => throw new Exception(s"Invalid SpecLite args")
  }

  def deserializeTask(task: String, deserializer: String => TaskDef) = {
    val taskDef = deserializer(task)
    val countTestSelectors = taskDef.selectors.toSeq.count {
      case _:TestSelector => true
      case _ => false
    }
    if (countTestSelectors == 0) rootTask(taskDef)
    else checkPropTask(taskDef)
  }

  def serializeTask(task: Task, serializer: TaskDef => String) =
    serializer(task.taskDef)

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = taskDefs.map(rootTask)

  abstract class BaseTask(override val taskDef: TaskDef) extends Task {
    val tags: Array[String] = Array()

    val props: Map[String,Prop] = {
      val fp = taskDef.fingerprint.asInstanceOf[SubclassFingerprint]
      val obj = if (fp.isModule) loadModule(taskDef.fullyQualifiedName,loader)
                else newInstance(taskDef.fullyQualifiedName, loader)(Seq())
      obj match {
        case props: Properties => Map(props.properties: _*)
        case prop: Prop => Map("" -> prop)
      }
    }

    def execute(handler: EventHandler, loggers: Array[Logger],
      continuation: Array[Task] => Unit
    ): Unit  = continuation(execute(handler,loggers))
  }

  def rootTask(td: TaskDef) = new BaseTask(td) {
    def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
      props.toArray map { case (name,_) =>
        checkPropTask(new TaskDef(td.fullyQualifiedName, td.fingerprint,
          td.explicitlySpecified, Array(new TestSelector(name)))
        )
      }
  }

  def checkPropTask(taskDef: TaskDef) = new BaseTask(taskDef) {
    val names = taskDef.selectors flatMap {
      case ts: TestSelector => Array(ts.testName)
      case _ => Array.empty[String]
    }

    def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
      names flatMap { name =>
        import shapeless.test.Pretty.{Params, pretty}

        val prop = props(name)
        val result = SpecLiteTest.check(params.withCustomClassLoader(Some(loader)), prop)

        val event = new Event {
          val status = result.status match {
            case SpecLiteTest.Passed => Status.Success
            case _:SpecLiteTest.Proved => Status.Success
            case _:SpecLiteTest.Failed => Status.Failure
            case SpecLiteTest.Exhausted => Status.Failure
            case _:SpecLiteTest.PropException => Status.Error
          }
          val throwable = result.status match {
            case SpecLiteTest.PropException(_, e, _) => new OptionalThrowable(e)
            case _:SpecLiteTest.Failed => new OptionalThrowable(
              new Exception(pretty(result, Params(0)))
            )
            case _ => new OptionalThrowable()
          }
          val fullyQualifiedName = taskDef.fullyQualifiedName
          val selector = new TestSelector(name)
          val fingerprint = taskDef.fingerprint
          val duration = -1L
        }

        handler.handle(event)

        event.status match {
          case Status.Success => successCount.incrementAndGet()
          case Status.Error => errorCount.incrementAndGet()
          case Status.Skipped => errorCount.incrementAndGet()
          case Status.Failure => failureCount.incrementAndGet()
          case _ => failureCount.incrementAndGet()
        }
        testCount.incrementAndGet()

        // TODO Stack traces should be reported through event
        val verbosityOpts = Set("-verbosity", "-v")
        val verbosity =
          args.grouped(2).filter(twos => verbosityOpts(twos.head))
          .toSeq.headOption.map(_.last).map(_.toInt).getOrElse(0)
        if(!result.passed) {
          val s = if (result.passed) GREEN + "+" + RESET else RED + "!"
          val n = if (name.isEmpty) taskDef.fullyQualifiedName else name.substring(1)
          val logMsg = s"$s $n ${pretty(result, Params(verbosity))}"
          loggers.foreach(l => l.info(logMsg))
        }
        Array.empty[Task]
      }
  }

}

final class SpecLiteFramework extends Framework {

  private def mkFP(mod: Boolean, cname: String, noArgCons: Boolean = true) =
    new SubclassFingerprint {
      def superclassName(): String = cname
      val isModule = mod
      def requireNoArgConstructor(): Boolean = noArgCons
    }

  val name = "SpecLite"

  def fingerprints: Array[Fingerprint] = Array(
    mkFP(false, "shapeless.SpecLite"),
    mkFP(true, "shapeless.SpecLite"),
    mkFP(false, "shapeless.test.SpecLite"),
    mkFP(true, "shapeless.test.SpecLite")
  )

  def runner(args: Array[String], remoteArgs: Array[String],
    loader: ClassLoader
  ): Runner = new SpecLiteRunner(args, remoteArgs, loader) {

    def receiveMessage(msg: String): Option[String] = msg(0) match {
      case 'd' =>
        val Array(t,s,f,e) = msg.tail.split(',')
        testCount.addAndGet(t.toInt)
        successCount.addAndGet(s.toInt)
        failureCount.addAndGet(f.toInt)
        errorCount.addAndGet(e.toInt)
        None
    }

    def done = {
      val heading = if (testCount.get == successCount.get) BLUE + "Passed" else RED + "Failed"
      s"$heading: Total $testCount, Failed $failureCount, Errors $errorCount, Passed $successCount"
    }
  }

  def slaveRunner(args: Array[String], remoteArgs: Array[String],
      loader: ClassLoader, send: String => Unit): Runner =
    new SpecLiteRunner(args, remoteArgs, loader) {
      def receiveMessage(msg: String) = None

      def done = {
        send(s"d$testCount,$successCount,$failureCount,$errorCount")
      ""
    }
  }
}
