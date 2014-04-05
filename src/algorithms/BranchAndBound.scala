package algorithms

import scala.collection.JavaConversions._
import core.ProjectCloner
import net.sf.mpxj.ProjectFile
import java.util.Date
import scala.collection.mutable.Buffer
import net.sf.mpxj.Task
import helpers.Printer
import BranchAndBound.Groups

object BranchAndBound {
    type Groups = List[(Date, List[Task])]
}

class BranchAndBound extends Algorithm {
    private def getCleanProject(project: ProjectFile) = {
        val clonedProject = ProjectCloner createBaseProject (project, true)
        clonedProject.getAllResourceAssignments.clear
        Algorithm fix (clonedProject)
    }

    override protected def perform(project: ProjectFile, byTime: Boolean = false) = {
        val cleanProject = getCleanProject (project)
        val groupedTasks = groupAndSortTasksByDate(cleanProject.getAllTasks)
        buildTreeWithProject(cleanProject) forGroups (groupedTasks)
    }

    private def groupAndSortTasksByDate(tasks: java.util.List[Task]) =
        tasks.groupBy(_ getStart).toList.sortBy(_ _1) map ((tuple) => (tuple _1, tuple._2 toList))

    private def buildTreeWithProject(project: ProjectFile) = new {
        def forGroups(groups: Groups): ProjectFile = {
            Printer groupedTasks (groups)
            buildPartialScheduler(project, 0, groups)
        }
    }

    private def buildPartialScheduler(currentSolution: ProjectFile, currentDepth: Int, groups: Groups): ProjectFile = {
        Printer projectCostAndDuration (currentSolution)
        val tasksToPermute = groups(currentDepth)._2 toList
        val permutedTasks = permuteTasksWithResources(tasksToPermute)
        val newSolution = chooseBestSolution(currentSolution, permutedTasks)
        if (currentDepth < groups.size - 1) buildPartialScheduler(newSolution, currentDepth + 1, groups) else newSolution
    }

    private def permuteTasksWithResources(tasks: List[Task]) = {
        List(tasks)
    }

    private def chooseBestSolution(currentSolution: ProjectFile, permutedTasks: List[List[Task]]) = {
        currentSolution
    }

}