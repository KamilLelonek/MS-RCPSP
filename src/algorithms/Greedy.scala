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
        var globalBestProject = cleanProject

        globalBestProject.getAllTasks foreach (task => {
            Printer projectCostAndDuration (globalBestProject)
            globalBestProject = assignBestResourceForTask(task) inProject (globalBestProject, byTime)
        })

        globalBestProject
    }

    private def assignBestResourceForTask(task: Task) = new {
        def inProject(globalBestProject: ProjectFile, byTime: Boolean) = {
            var localBestProject: ProjectFile = operateOnCopy(globalBestProject, task)

            val resourcesCapablePerformingTask = SkillsUtilities resourcesCapablePerformingTask (task)
            resourcesCapablePerformingTask foreach (resource => {
                val localTempProject = operateOnCopy(globalBestProject, task, resource)
                localBestProject = chooseBetterProject(localBestProject, localTempProject, byTime)
            })

            localBestProject
        }
    }

    private def operateOnCopy(globalBestProject: ProjectFile, task: Task, resource: Resource = null) = {
        val localTempProject = ProjectCloner createBaseProject (globalBestProject, true)
        val localTempTask = localTempProject getTaskByID (task getID)
        val localTempResource =
            if (resource != null)
                localTempProject getResourceByID (resource getID)
            else
                SkillsUtilities resourcesCapablePerformingTask (localTempTask) get (0)
        assignResource (localTempResource) toTask (localTempTask)
        Algorithm fix (localTempProject)
    }

    private def chooseBetterProject(localBestProject: ProjectFile, localTempProject: ProjectFile, byTime: Boolean): ProjectFile =
        calculateBetterProject(localTempProject, localBestProject) byEval (if (byTime) Eval getProjectDuration else Eval getProjectCost)

    private def calculateBetterProject(firstProject: ProjectFile, lastProject: ProjectFile) = new {
        def byEval(eval: ProjectFile => Double) = List(firstProject, lastProject) minBy (eval(_))
    }

}