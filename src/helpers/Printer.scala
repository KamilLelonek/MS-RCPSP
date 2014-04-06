package helpers

import java.text.SimpleDateFormat
import algorithms.Algorithm
import algorithms.BranchAndBound
import algorithms.OptimizationMethod
import core.eval.Eval
import net.sf.mpxj.ProjectFile
import java.io.File

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

    def projectName(project: (File, ProjectFile)) = println(s"\nReading ${project._1}")

    def time(function: => Unit) = {
        val startTime = System.currentTimeMillis
        function
        val endTime = System.currentTimeMillis
        println(s"Elapsed time: ${endTime - startTime} ms")
    }
}