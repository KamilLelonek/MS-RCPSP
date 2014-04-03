package algorithms

import scala.collection.JavaConversions.asScalaBuffer

import core.ProjectCloner
import net.sf.mpxj.ProjectFile

class BranchAndBound extends Algorithm {

    override protected def perform(project: ProjectFile, byTime: Boolean = false) = {
        val cleanProject = getCleanProject (project)
        val groupedProjects = cleanProject.getAllTasks.groupBy(_ getStart).toSeq.sortBy(_ _1)
        groupedProjects.foreach(println)
        null
    }

    private def getCleanProject(project: ProjectFile) = {
        val clonedProject = ProjectCloner createBaseProject (project, true)
        clonedProject.getAllResourceAssignments.clear
        Algorithm fix (clonedProject)
    }
}