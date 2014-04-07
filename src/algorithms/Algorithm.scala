package algorithms

import core.conflicts.ConflictFixer
import core.eval.Eval
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.Resource
import net.sf.mpxj.Task
import core.ProjectCloner
import core.criticalpath.CriticalPathFixer

sealed abstract class OptimizationMethod
case object time extends OptimizationMethod
case object cost extends OptimizationMethod

object Algorithm {
    def packTasksAndFixResourcesConflicts(project: ProjectFile) = {
        ConflictFixer pack (project)
        ConflictFixer fixConflicts (project)
        project
    }

    def rescheduleTasksByStartDate(project: ProjectFile) = {
        val clonedProject = cloneProject(project)
        clonedProject.getAllResourceAssignments.clear
        ConflictFixer pack (clonedProject)
        CriticalPathFixer rescheduleProject (clonedProject)
        clonedProject
    }

    def cloneProject(project: ProjectFile, withAssignments: Boolean = false) =
        ProjectCloner createBaseProject (project, withAssignments)
}

abstract class Algorithm {
    def optimize(project: ProjectFile) = new {
        def by(optimizationMethod: OptimizationMethod): ProjectFile = {
            optimizationMethod match {
                case `time` => perform(project, true)
                case `cost` => perform(project)
            }
        }
    }

    protected def perform(cleanProject: ProjectFile, byTime: Boolean = false): ProjectFile

    protected def assignResource(resource: Resource) = new {
        def toTask(task: Task) = {
            val resourceAssignment = task addResourceAssignment (resource)
            resourceAssignment setStart (task getStart)
            resourceAssignment setWork (task getDuration)
            resourceAssignment setRemainingWork (resourceAssignment getWork)
            resourceAssignment setCost (resourceAssignment.getWork.getDuration * resource.getStandardRate.getAmount)
        }
    }

    protected def chooseBetterProject(localBestProject: ProjectFile, localTempProject: ProjectFile, byTime: Boolean): ProjectFile =
        if (localBestProject == null) localTempProject
        else
            calculateBetterProject(localBestProject, localTempProject) byEval (if (byTime) Eval getProjectDuration else Eval getProjectCost)

    private def calculateBetterProject(firstProject: ProjectFile, lastProject: ProjectFile) = new {
        def byEval(eval: ProjectFile => Double) = List(firstProject, lastProject) minBy (eval(_))
    }
}