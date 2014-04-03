package helpers

import scala.collection.JavaConversions.asScalaBuffer
import core.eval.Eval
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.Task
import algorithms.OptimizationMethod
import algorithms.cost
import algorithms.time
import algorithms.Algorithm

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

    def methodHeader(optimizationMethod: OptimizationMethod) = optimizationMethod match {
        case `time` => println("-------- Time optimization --------\n")
        case `cost` => println("\n-------- Cost optimization --------")
    }

    def algorithmHeader[T <: Algorithm](algorithm: T) =
        println(s"Algorithm ${algorithm.getClass.getName} optimalization")
}