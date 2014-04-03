package algorithms

import core.ProjectCloner
import core.conflicts.ConflictFixer
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.Resource
import net.sf.mpxj.Task

sealed abstract class OptimizationMethod
case object time extends OptimizationMethod
case object cost extends OptimizationMethod

object Algorithm {
    def fix(project: ProjectFile) = {
        ConflictFixer pack (project)
        ConflictFixer fixConflicts (project)
        project
    }
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
}