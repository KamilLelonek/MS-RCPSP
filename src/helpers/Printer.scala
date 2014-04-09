package helpers

import java.io.File
import java.text.SimpleDateFormat
import algorithms.Algorithm
import algorithms.BranchAndBound
import algorithms.OptimizationMethod
import core.eval.Eval
import net.sf.mpxj.ProjectFile
import scala.collection.mutable.StringBuilder
import java.io.PrintWriter

object Printer {
    private lazy val dateFormat = new SimpleDateFormat("dd MMMM y',' HH:mm:ss")
    private lazy val stringBuilder = new StringBuilder

    def projectCostAndDuration(project: ProjectFile) =
        log(s"\t\tProject cost: ${Eval getProjectCost project} and duration: ${Eval getProjectDuration project}")

    def algorithmHeader(algorithm: Algorithm) = new {
        log(s"\n### ${IO simpleClassName algorithm} algorithm ###")
        def andMethodHeader(optimizationMethod: OptimizationMethod) =
            log(s"\t-------- ${IO simpleClassName optimizationMethod} optimization --------")
    }

    def groupedTasks(groups: BranchAndBound.Groups) = {
        log(s"\nGroups count: ${groups size}")
        groups.zipWithIndex foreach {
            case (group, index) =>
                log(s"Node ${index}: ${dateFormat format (group _1)} with tasks: ${group _2}")
        }
    }

    def projectName(project: (File, ProjectFile)) =
        log(s"\n================> Reading ${project _1} <================")

    def time(function: => Unit) = {
        val startTime = System.currentTimeMillis
        function
        val endTime = System.currentTimeMillis
        log(s"\t\tElapsed time: ${endTime - startTime} ms")
    }

    def log(string: String) = {
        stringBuilder append s"${string}\n"
        println(string)
    }

    def flush = {
        val printWriter = new PrintWriter(new File("results/log.txt"))
        printWriter write (stringBuilder toString)
        printWriter close
    }
}