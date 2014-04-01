package helpers

import net.sf.mpxj.mpp.MPPReader

object Reader {
    private lazy val reader = new MPPReader

    def getAllTasks(sourceFile: String) = {
        val projectFile = reader.read(sourceFile)
        val projectTasks = projectFile.getAllTasks
        if (projectTasks.get(0).getName.equals("clone")) projectTasks.remove(0)
        projectTasks
    }
}