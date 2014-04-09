import algorithms.Algorithm
import algorithms.BranchAndBound
import algorithms.Greedy
import algorithms.OptimizationMethod
import algorithms.cost
import algorithms.time
import helpers.IO
import helpers.Printer
import net.sf.mpxj.ProjectFile
import java.io.File

object Main extends App {
    performAllAlgorithmsForFiles(IO myProjects)

    def performAllAlgorithmsForFiles(projects: Array[(File, ProjectFile)] = IO allProjects) = {
        projects.foreach(project => {
            Printer projectName project
            performAllAlgorithmsForFile (project)
        })
        Printer flush
    }

    def performAllAlgorithmsForFile(project: (File, ProjectFile)) = {
        val methods = Array(time, cost)
        val algorithms = Array(new Greedy, new BranchAndBound)
        for (algorithm <- algorithms; method <- methods) {
            Printer time (performAlgorithm(algorithm) forProject (project _2) withMethod method forFile (project _1))
        }
    }

    def performAlgorithm(algorithm: Algorithm) = new {
        def forProject(project: ProjectFile) = new {
            def withMethod(method: OptimizationMethod) = new {
                def forFile(file: File) = {
                    Printer algorithmHeader algorithm andMethodHeader method
                    val result = algorithm optimize project by method
                    Printer projectCostAndDuration result
                    IO writeProject result fromAlgorithm algorithm andMethod method forFile file
                }
            }
        }
    }
}