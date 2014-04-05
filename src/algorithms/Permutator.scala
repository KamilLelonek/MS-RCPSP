package algorithms

object Permutator {
    /*
     *  task: (taskId: Integer, taskResources: List[taskId: Integer])
     */

    type Task = (Integer, List[Integer])

    def permutateTasks(t: List[Task]): List[List[(Integer, Integer)]] = {
        t.size match {
            case 1  => permutate(t(0))
            case 2  => permutate(t(0), t(1))
            case 3  => permutate(t(0), t(1), t(2))
            case 4  => permutate(t(0), t(1), t(2), t(3))
            case 5  => permutate(t(0), t(1), t(2), t(3), t(4))
            case 6  => permutate(t(0), t(1), t(2), t(3), t(4), t(5))
            case 7  => permutate(t(0), t(1), t(2), t(3), t(4), t(5), t(6))
            case 8  => permutate(t(0), t(1), t(2), t(3), t(4), t(5), t(6), t(7))
            case 9  => permutate(t(0), t(1), t(2), t(3), t(4), t(5), t(6), t(7), t(8))
            case 10 => permutate(t(0), t(1), t(2), t(3), t(4), t(5), t(6), t(7), t(8), t(9))
            case 11 => permutate(t(0), t(1), t(2), t(3), t(4), t(5), t(6), t(7), t(8), t(9), t(10))
            case 12 => permutate(t(0), t(1), t(2), t(3), t(4), t(5), t(6), t(7), t(8), t(9), t(10), t(11))
            case 13 => permutate(t(0), t(1), t(2), t(3), t(4), t(5), t(6), t(7), t(8), t(9), t(10), t(11), t(12))
            case 14 => permutate(t(0), t(1), t(2), t(3), t(4), t(5), t(6), t(7), t(8), t(9), t(10), t(11), t(12), t(13))
            case 15 => permutate(t(0), t(1), t(2), t(3), t(4), t(5), t(6), t(7), t(8), t(9), t(10), t(11), t(12), t(13), t(14))
            case _  => throw new IllegalArgumentException(s"Wrong tasks size: ${t.size}")
        }
    }

    private def permutate(t1: Task) = {
        for {
            a <- t1._2
        } yield List((t1._1, a))
    }

    private def permutate(t1: Task, t2: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
        } yield List((t1._1, a1), (t2._1, a2))
    }

    private def permutate(t1: Task, t2: Task, t3: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task, t7: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
            a7 <- t7._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6), (t7._1, a7))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task, t7: Task, t8: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
            a7 <- t7._2
            a8 <- t8._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6), (t7._1, a7), (t8._1, a8))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task, t7: Task, t8: Task, t9: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
            a7 <- t7._2
            a8 <- t8._2
            a9 <- t9._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6), (t7._1, a7), (t8._1, a8), (t9._1, a9))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task, t7: Task, t8: Task, t9: Task, t10: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
            a7 <- t7._2
            a8 <- t8._2
            a9 <- t9._2
            a10 <- t10._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6), (t7._1, a7), (t8._1, a8), (t9._1, a9), (t10._1, a10))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task, t7: Task, t8: Task, t9: Task, t10: Task, t11: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
            a7 <- t7._2
            a8 <- t8._2
            a9 <- t9._2
            a10 <- t10._2
            a11 <- t11._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6), (t7._1, a7), (t8._1, a8), (t9._1, a9), (t10._1, a10), (t11._1, a11))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task, t7: Task, t8: Task, t9: Task, t10: Task, t11: Task, t12: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
            a7 <- t7._2
            a8 <- t8._2
            a9 <- t9._2
            a10 <- t10._2
            a11 <- t11._2
            a12 <- t12._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6), (t7._1, a7), (t8._1, a8), (t9._1, a9), (t10._1, a10), (t11._1, a11), (t12._1, a12))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task, t7: Task, t8: Task, t9: Task, t10: Task, t11: Task, t12: Task, t13: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
            a7 <- t7._2
            a8 <- t8._2
            a9 <- t9._2
            a10 <- t10._2
            a11 <- t11._2
            a12 <- t12._2
            a13 <- t13._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6), (t7._1, a7), (t8._1, a8), (t9._1, a9), (t10._1, a10), (t11._1, a11), (t12._1, a12), (t13._1, a13))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task, t7: Task, t8: Task, t9: Task, t10: Task, t11: Task, t12: Task, t13: Task, t14: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
            a7 <- t7._2
            a8 <- t8._2
            a9 <- t9._2
            a10 <- t10._2
            a11 <- t11._2
            a12 <- t12._2
            a13 <- t13._2
            a14 <- t14._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6), (t7._1, a7), (t8._1, a8), (t9._1, a9), (t10._1, a10), (t11._1, a11), (t12._1, a12), (t13._1, a13), (t14._1, a14))
    }

    private def permutate(t1: Task, t2: Task, t3: Task, t4: Task, t5: Task, t6: Task, t7: Task, t8: Task, t9: Task, t10: Task, t11: Task, t12: Task, t13: Task, t14: Task, t15: Task) = {
        for {
            a1 <- t1._2
            a2 <- t2._2
            a3 <- t3._2
            a4 <- t4._2
            a5 <- t5._2
            a6 <- t6._2
            a7 <- t7._2
            a8 <- t8._2
            a9 <- t9._2
            a10 <- t10._2
            a11 <- t11._2
            a12 <- t12._2
            a13 <- t13._2
            a14 <- t14._2
            a15 <- t15._2
        } yield List((t1._1, a1), (t2._1, a2), (t3._1, a3), (t4._1, a4), (t5._1, a5), (t6._1, a6), (t7._1, a7), (t8._1, a8), (t9._1, a9), (t10._1, a10), (t11._1, a11), (t12._1, a12), (t13._1, a13), (t14._1, a14), (t15._1, a15))
    }
}