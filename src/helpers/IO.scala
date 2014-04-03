package helpers

import java.io.File

import scala.Array.canBuildFrom

import algorithms.Algorithm
import algorithms.OptimizationMethod
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.mpp.MPPReader
import net.sf.mpxj.mspdi.MSPDIWriter

object IO {
    private lazy val reader = new MPPReader
    private lazy val writer = new MSPDIWriter
    private lazy val allFiles = new File("mpp") listFiles

    def firstProject = readProject(allFiles(0) getPath)

    def allProjects = allFiles map (projectPath => readProject(projectPath getPath))

    def readProject(sourcePath: String) = {
        println("Reading from " + sourcePath)
        reader read (sourcePath)
    }

    def writeProject(project: ProjectFile) = new {
        def fromAlgorithm(algorithm: Algorithm) = new {
            def andMethod(method: OptimizationMethod) = new {
                val destinationPath = pathName(algorithm, method)
                println("\nWriting to " + destinationPath)
                writer write (project, destinationPath)
            }
        }
    }

    def pathName(algorithm: Algorithm, method: OptimizationMethod) =
        s"results/${simpleClassName(algorithm)}_${simpleClassName(method)}.xml"
    def simpleClassName(instance: AnyRef) = instance.getClass.getSimpleName.toLowerCase.stripSuffix("$")
}