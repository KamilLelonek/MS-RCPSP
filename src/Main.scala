import algorithms.Greedy
import algorithms._
import core.ProjectCloner
import helpers.IO
import helpers.Printer

object Main extends App {
    val methods = Array(time) //, cost)
    val algorithms = Array(new Greedy) //, new BranchAndBound)
    val project = IO readFirstProject

    for (algorithm <- algorithms; method <- methods) {
        Printer algorithmHeader (algorithm) andMethodHeader (method)
        var result = algorithm.optimize(project) by (method)
        Printer projectCostAndDuration (result)
        IO writeProject (result) fromAlgorithm (algorithm) andMethod (method)
    }
}