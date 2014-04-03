import algorithms.Greedy
import algorithms.cost
import algorithms.time
import helpers.IO
import helpers.Printer
import algorithms.BranchAndBound

object Main extends App {
    val methods = Array(time, cost)
    val algorithms = Array( /*new Greedy),*/ new BranchAndBound)
    val project = IO firstProject

    //    projects.foreach(project => {
    for (algorithm <- algorithms; method <- methods) {
        Printer algorithmHeader (algorithm) andMethodHeader (method)
        var result = algorithm.optimize(project) by (method)
        Printer projectCostAndDuration (result)
        IO writeProject (result) fromAlgorithm (algorithm) andMethod (method)
    }
    //    })

}