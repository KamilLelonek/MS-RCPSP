package helpers

import net.sf.mpxj.Task
import scala.collection.JavaConversions._

object Printer {
    def listHierarchy(task: Task, indent: String = ""): Unit = {
        task.getChildTasks.foreach(child => {
            println(indent + "Task: " + child.getName)
            listHierarchy(child, indent + "\t")
        })
    }
}