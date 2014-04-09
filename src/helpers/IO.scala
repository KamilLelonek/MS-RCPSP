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

    def myProjects = mapFilesOnProjects(Array(new File("mpp/10_3_5_3.mpp"), new File("mpp/10_5_8_5.mpp")))
    def allProjects = mapFilesOnProjects(allFiles)
    private def mapFilesOnProjects(files: Array[File]) = files map (projectPath => (projectPath, readProject(projectPath getPath)))

    private def readProject(sourcePath: String) = reader read sourcePath

    def writeProject(project: ProjectFile) = new {
        def fromAlgorithm(algorithm: Algorithm) = new {
            def andMethod(method: OptimizationMethod) = new {
                def forFile(sourceFile: File) = {
                    val destinationPath = pathName(algorithm, method, sourceFile)
                    Printer log "\n\t\tWriting to " + destinationPath
                    writer write (project, destinationPath)
                }
            }
        }
    }

    private def withoutExtension(file: File) = file.getName substring (0, file.getName indexOf ".")

    def pathName(algorithm: Algorithm, method: OptimizationMethod, sourceFile: File) =
        s"results/${withoutExtension(sourceFile)}-${simpleClassName(algorithm)}_${simpleClassName(method)}.xml"
    def simpleClassName(instance: AnyRef) = instance.getClass.getSimpleName.toLowerCase.stripSuffix("$")
}