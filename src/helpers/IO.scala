package helpers

import java.io.File

import core.ProjectCloner
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.mpp.MPPReader
import net.sf.mpxj.mspdi.MSPDIWriter

object IO {
    private lazy val reader = new MPPReader
    private lazy val writer = new MSPDIWriter
    
    lazy val allFiles = new File("mpp") listFiles

    def readFirstProject = readProject(allFiles(0) getPath)

    def readProject(sourcePath: String) = {
        val projectFile = reader read (sourcePath)
        ProjectCloner createBaseProject (projectFile, false)
    }

    def writeProject(project: ProjectFile) = new {
        def to(destinationPath: String) = {
            writer write (project, destinationPath)
        }
    }
}