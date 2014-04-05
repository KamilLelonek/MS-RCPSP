package helpers

import scala.collection.JavaConversions.asScalaBuffer
import algorithms.Algorithm
import algorithms.OptimizationMethod
import algorithms.cost
import algorithms.time
import core.eval.Eval
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.Task
import java.util.Date
import scala.collection.mutable.Buffer
import java.text.SimpleDateFormat
import algorithms.BranchAndBound

object Printer {
    private lazy val dateFormat = new SimpleDateFormat("dd MMMM y',' HH:mm:ss")

    def projectCostAndDuration(project: ProjectFile) =
        println(s"Project cost: ${Eval.getProjectCost(project)} and duration: ${Eval.getProjectDuration(project)}")

    def algorithmHeader(algorithm: Algorithm) = new {
        println(s"\n### ${IO simpleClassName (algorithm)} algorithm ###")
        def andMethodHeader(optimizationMethod: OptimizationMethod) =
            println(s"-------- ${IO simpleClassName (optimizationMethod)} optimization --------")
    }

    def groupedTasks(groups: BranchAndBound.Groups) = {
        println(s"\nGroups count: ${groups.size}")
        groups.zipWithIndex foreach {
            case (group, index) =>
                println(s"Node ${index}: ${dateFormat.format(group._1)} with tasks: ${group._2}")
        }
    }
}