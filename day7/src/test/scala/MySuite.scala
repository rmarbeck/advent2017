// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day7 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "mwzaxaj")
    assertEquals(score2, "1219")

  test("Day7 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "tknk")
    assertEquals(score2, "60")
