package algorithms

import java.util.Date

import scala.collection.JavaConversions.asScalaBuffer

import core.ProjectCloner
import core.SkillsUtilities
import core.conflicts.ConflictFixer
import core.criticalpath.CriticalPathFixer
import helpers.Printer
import net.sf.mpxj.ProjectFile
import net.sf.mpxj.Task

object BranchAndBound {
    type Groups = List[(Date, List[Integer])]
}

class BranchAndBound extends Algorithm {

    override protected def perform(project: ProjectFile, byTime: Boolean = false) = {
        val cleanProject = cloneProjectWithoutAssignments (project)
        val groupedTasksIDs = groupAndSortTasksByStartDate(cleanProject getAllTasks)
        buildTreeWithProject(cleanProject) forGroups (groupedTasksIDs) byTime (byTime)
    }

    private def cloneProjectWithoutAssignments(project: ProjectFile) = {
        val clonedProject = ProjectCloner createBaseProject (project, false)
        clonedProject.getAllResourceAssignments.clear
        ConflictFixer pack (clonedProject)
        CriticalPathFixer rescheduleProject (clonedProject)
        clonedProject
    }

    private def groupAndSortTasksByStartDate(tasks: java.util.List[Task]) =
        tasks.groupBy(_ getStart).toList.sortBy(_ _1) map ((tuple) => (tuple _1, tuple._2.toList map ((task) => task getID)))

    private def buildTreeWithProject(project: ProjectFile) = new {
        def forGroups(groups: BranchAndBound.Groups) = new {
            def byTime(byTime: Boolean): ProjectFile =
                buildPartialScheduler(project, 0, groups, byTime)
        }
    }

    private def buildPartialScheduler(currentSolution: ProjectFile, currentDepth: Int, groups: BranchAndBound.Groups, byTime: Boolean): ProjectFile = {
        Printer projectCostAndDuration (currentSolution)
        val tasksIdsToPermute = groups(currentDepth)._2 toList
        val tasksWithResources = getAllTasksResources(tasksIdsToPermute) inProject (currentSolution)
        val permutedTasks = permutateTasks (tasksWithResources)
        val newSolution = chooseBestSolution(currentSolution, permutedTasks, byTime)
        if (currentDepth < groups.size - 1) buildPartialScheduler(newSolution, currentDepth + 1, groups, byTime) else newSolution
    }

    def permutateTasks(tasksWithResources: List[(Integer, List[Integer])], currentSolution: List[(Integer, Integer)] = Nil): List[List[(Integer, Integer)]] =
        tasksWithResources match {
            case head :: Nil  => head._2.map (x => ((head._1, x) :: currentSolution).reverse)
            case head :: tail => head._2.flatMap(x => permutateTasks(tail, (head._1, x) :: currentSolution))
        }

    def getAllTasksResources(tasksIDs: List[Integer]) = new {
        def inProject(project: ProjectFile) =
            tasksIDs map (project getTaskByID) map (getTaskWithResources)
    }

    private def getTaskWithResources(task: Task) =
        (task getID, SkillsUtilities.resourcesCapablePerformingTask(task).map(_ getID).toList)

    private def chooseBestSolution(currentSolution: ProjectFile, possibleSolutions: List[List[(Integer, Integer)]], byTime: Boolean) = {
        var localBestProject: ProjectFile = null
        possibleSolutions foreach ((solution) => {
            val localTempProject = ProjectCloner createBaseProject (currentSolution, true)
            solution foreach ((taskWithResource) => {
                val localTempTask = localTempProject getTaskByID (taskWithResource _1)
                val localTempResource = localTempProject getResourceByID (taskWithResource _2)
                assignResource (localTempResource) toTask (localTempTask)
                if (byTime) Algorithm fix (localTempProject)
                localBestProject = chooseBetterProject(localBestProject, localTempProject, byTime)
            })
        })
        localBestProject
    }
}