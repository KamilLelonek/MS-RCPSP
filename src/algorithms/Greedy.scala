package algorithms

import scala.collection.JavaConversions.asScalaBuffer

import core.ProjectCloner
import core.SkillsUtilities
import core.eval.Eval
import helpers.Printer
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.Resource
import net.sf.mpxj.Task

class Greedy extends Algorithm {

    override protected def perform(cleanProject: ProjectFile, byTime: Boolean = false) = {
        cleanProject.getAllTasks.foldLeft(cleanProject)((globalBestProject, task) => {
            Printer projectCostAndDuration (globalBestProject)
            assignBestResourceForTask(task) inProject (globalBestProject, byTime)
        })
    }

    private def assignBestResourceForTask(task: Task) = new {
        def inProject(globalBestProject: ProjectFile, byTime: Boolean) = {
            val resourcesCapablePerformingTask = SkillsUtilities resourcesCapablePerformingTask (task)
            resourcesCapablePerformingTask.foldLeft(null: ProjectFile)((localBestProject, resource) => {
                val localTempProject = operateOnCopy(globalBestProject, task, resource)
                chooseBetterProject(localBestProject, localTempProject, byTime)
            })
        }
    }

    private def operateOnCopy(globalBestProject: ProjectFile, task: Task, resource: Resource) = {
        val localTempProject = ProjectCloner createBaseProject (globalBestProject, true)
        val localTempTask = localTempProject getTaskByID (task getID)
        val localTempResource = localTempProject getResourceByID (resource getID)
        assignResource (localTempResource) toTask (localTempTask)
        Algorithm fix (localTempProject)
    }

    private def chooseBetterProject(localBestProject: ProjectFile, localTempProject: ProjectFile, byTime: Boolean): ProjectFile =
        if (localBestProject == null) localTempProject
        else
            calculateBetterProject(localBestProject, localTempProject) byEval (if (byTime) Eval getProjectDuration else Eval getProjectCost)

    private def calculateBetterProject(firstProject: ProjectFile, lastProject: ProjectFile) = new {
        def byEval(eval: ProjectFile => Double) = List(firstProject, lastProject) minBy (eval(_))
    }

}