package algorithms

import scala.collection.JavaConversions.asScalaBuffer

import core.ProjectCloner
import core.SkillsUtilities
import core.eval.Eval
import helpers.Printer
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.Task

class Greedy extends Algorithm {

    override protected def perform(cleanProject: ProjectFile, byTime: Boolean = false) = {
        var bestProject = cleanProject
        bestProject.getAllTasks foreach (task => {
            Printer projectCostAndDuration (bestProject)
            bestProject = assignBestResourceForTask(task) inProject (bestProject, byTime)
        })
        bestProject
    }

    private def assignBestResourceForTask(task: Task) = new {
        def inProject(bestProject: ProjectFile, byTime: Boolean) = {
            var localBestProject: ProjectFile = null

            var localTempProject = ProjectCloner createBaseProject (bestProject, true)
            var localTempTask = localTempProject getTaskByID (task getUniqueID)
            val resourcesCapablePerformingTask = SkillsUtilities resourcesCapablePerformingTask (localTempTask)

            resourcesCapablePerformingTask foreach (resource => {
                var localTempResource = localTempProject getResourceByID (resource getUniqueID)
                assignResource(localTempResource) toTask (localTempTask)
                localBestProject = chooseBestProject(localBestProject, localTempProject, byTime)
                localTempProject = ProjectCloner createBaseProject (bestProject, true)
                localTempTask = localTempProject getTaskByID (task getUniqueID)
            })

            localBestProject
        }
    }

    private def chooseBestProject(localBestProject: ProjectFile, localTempProject: ProjectFile, byTime: Boolean): ProjectFile = {
        if (localBestProject == null) return localTempProject
        if (byTime) {
            Algorithm fix (localTempProject)
            chooseBetterProject(localTempProject, localBestProject) byEval (Eval.getProjectCost)
        }
        else {
            chooseBetterProject(localTempProject, localBestProject) byEval (Eval.getProjectDuration)
        }
    }

    private def chooseBetterProject(firstProject: ProjectFile, lastProject: ProjectFile) = new {
        def byEval(eval: ProjectFile => Double) = {
            if (eval(firstProject) > eval(lastProject)) lastProject else firstProject
        }
    }

}