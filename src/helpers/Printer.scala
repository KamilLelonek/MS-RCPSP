package helpers

import scala.collection.JavaConversions.asScalaBuffer

import core.eval.Eval
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.Task

object Printer {
    def listHierarchy(task: Task, indent: String = ""): Unit = {
        task.getChildTasks foreach (child => {
            println(indent + "Task: " + child.getName)
            listHierarchy(child, indent + "\t")
        })
    }

    def projectCost(project: ProjectFile) =
        println("Project cost: " + Eval.getProjectCost(project))

    def projectDuration(project: ProjectFile) =
        println("Project duration: " + Eval.getProjectDuration(project))
}