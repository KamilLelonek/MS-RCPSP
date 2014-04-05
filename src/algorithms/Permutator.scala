package algorithms

object Permutator {
    /*
     *  task: (taskId: Integer, taskResources: List[taskId: Integer])
     */

    def permutateTasks(t: List[(Integer, List[Integer])]): List[List[(Integer, Integer)]] = {
        t.size match {
            case 1 => permutate(t(0))
            case 2 => permutate(t(0), t(1))
            case 3 => permutate(t(0), t(1), t(2))
            case 4 => permutate(t(0), t(1), t(2), t(3))
            case 5 => permutate(t(0), t(1), t(2), t(3), t(4))
            case 6 => permutate(t(0), t(1), t(2), t(3), t(4), t(5))
            case _ => null
        }
    }

    private def permutate(t1: (Integer, List[Integer])) = {
        for {
            a <- t1._2
        } yield List((t1._1, a))
    }

    private def permutate(t1: (Integer, List[Integer]), t2: (Integer, List[Integer])) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
        } yield List((t1._1, a1), (t2._1, a2))
    }

    private def permutate(t1: (Integer, List[Integer]), t2: (Integer, List[Integer]), t3: (Integer, List[Integer])) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3))
    }

    private def permutate(t1: (Integer, List[Integer]), t2: (Integer, List[Integer]), t3: (Integer, List[Integer]), t4: (Integer, List[Integer])) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4))
    }

    private def permutate(t1: (Integer, List[Integer]), t2: (Integer, List[Integer]), t3: (Integer, List[Integer]), t4: (Integer, List[Integer]), t5: (Integer, List[Integer])) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5))
    }

    private def permutate(t1: (Integer, List[Integer]), t2: (Integer, List[Integer]), t3: (Integer, List[Integer]), t4: (Integer, List[Integer]), t5: (Integer, List[Integer]), t6: (Integer, List[Integer])) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6))
    }
}