// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day5 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "388611")
    assertEquals(score2, "27763113")

  test("Day5 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "5")
    assertEquals(score2, "10")
