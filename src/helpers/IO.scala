package helpers

import java.io.File
import core.ProjectCloner
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.mpp.MPPReader
import net.sf.mpxj.mspdi.MSPDIWriter
import algorithms.Algorithm
import algorithms.OptimizationMethod

object IO {
    private lazy val reader = new MPPReader
    private lazy val writer = new MSPDIWriter

    lazy val allFiles = new File("mpp") listFiles

    def readFirstProject = readProject(allFiles(0) getPath)

    def readProject(sourcePath: String) = {
        println("Reading from " + sourcePath)
        reader read (sourcePath)
    }

    def writeProject(project: ProjectFile) = new {
        def fromAlgorithm[T <: Algorithm](algorithm: T) = new {
            def andMethod(method: OptimizationMethod) = new {
                val destinationPath = s"${simpleClassName(algorithm)}${simpleClassName(method)}.xml"
                println("Writing to " + destinationPath)
                writer write (project, destinationPath)
            }
        }
    }

    private def simpleClassName(obj: AnyRef) = obj.getClass.getSimpleName.capitalize
}