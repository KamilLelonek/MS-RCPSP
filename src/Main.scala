import algorithms.Algorithm
import algorithms.BranchAndBound
import algorithms.Greedy
import algorithms.OptimizationMethod
import algorithms.cost
import algorithms.time
import helpers.IO
import helpers.Printer
import net.sf.mpxj.ProjectFile

object Main extends App {
    lazy val methods = Array(time, cost)
    lazy val algorithms = Array(new Greedy, new BranchAndBound)
    lazy val projects = IO allProjects
    lazy val firstProject = IO firstProject

    performAllAlgorithmsForFile(firstProject)

    def performAllAlgorithmsForAllFiles = {
        projects.foreach(project => {
            performAllAlgorithmsForFile (project)
        })
    }

    def performAllAlgorithmsForFile(project: ProjectFile) = {
        for (algorithm <- algorithms; method <- methods) {
            performAlgorithm(algorithm) forProject (project) withMethod (method)
        }
    }

    def performAlgorithm(algorithm: Algorithm) = new {
        def forProject(project: ProjectFile) = new {
            def withMethod(method: OptimizationMethod) = {
                Printer algorithmHeader (algorithm) andMethodHeader (method)
                val result = algorithm.optimize(project) by (method)
                Printer projectCostAndDuration (result)
                IO writeProject (result) fromAlgorithm (algorithm) andMethod (method)
            }
        }
    }
}