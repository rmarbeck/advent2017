// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day21 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "173")
    assertEquals(score2, "2456178")

  test("Day21 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "22")
    assertEquals(score2, "22")
