// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day18 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "1187")
    assertEquals(score2, "5969")

  test("Day18 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "4")
    assertEquals(score2, "1")
