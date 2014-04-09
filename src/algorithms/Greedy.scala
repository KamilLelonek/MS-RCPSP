package algorithms

import scala.collection.JavaConversions.asScalaBuffer

import Algorithm.cloneProject
import core.SkillsUtilities
import helpers.Printer
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.Resource
import net.sf.mpxj.Task

class Greedy extends Algorithm {

    override protected def perform(project: ProjectFile, byTime: Boolean = false) = {
        val cleanProject = Algorithm packTasksAndFixResourcesConflicts (cloneProject (project))
        cleanProject.getAllTasks.foldLeft(cleanProject)((globalBestProject, task) => {
            Printer projectCostAndDuration globalBestProject
            assignBestResourceForTask (task) inProject (globalBestProject, byTime)
        })
    }

    private def assignBestResourceForTask(task: Task) = new {
        def inProject(globalBestProject: ProjectFile, byTime: Boolean) = {
            val resourcesCapablePerformingTask = SkillsUtilities resourcesCapablePerformingTask (task)
            resourcesCapablePerformingTask.foldLeft(null: ProjectFile)((localBestProject, resource) => {
                val localTempProject = operateOnCopy(globalBestProject, task, resource)
                chooseBetterProject(localBestProject, localTempProject)(byTime)
            })
        }
    }

    private def operateOnCopy(globalBestProject: ProjectFile, task: Task, resource: Resource) = {
        val localTempProject = cloneProject (globalBestProject, true)
        val localTempTask = localTempProject getTaskByID (task getID)
        val localTempResource = localTempProject getResourceByID (resource getID)
        assignResource (localTempResource) toTask localTempTask
        Algorithm packTasksAndFixResourcesConflicts localTempProject
    }
}