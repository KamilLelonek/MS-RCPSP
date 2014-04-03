package helpers

import scala.collection.JavaConversions.asScalaBuffer

import algorithms.Algorithm
import algorithms.OptimizationMethod
import algorithms.cost
import algorithms.time
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

    def projectCostAndDuration(project: ProjectFile) =
        println(s"Project cost: ${Eval.getProjectCost(project)} and duration: ${Eval.getProjectDuration(project)}")

    def algorithmHeader[T <: Algorithm](algorithm: T) = new {
        println(s"\n### ${IO simpleClassName (algorithm)} algorithm ###")
        def andMethodHeader(optimizationMethod: OptimizationMethod) =
            println(s"-------- ${IO simpleClassName (optimizationMethod)} optimization --------")
    }
}