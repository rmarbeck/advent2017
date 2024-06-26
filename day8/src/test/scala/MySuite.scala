// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day8 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "6343")
    assertEquals(score2, "7184")

  test("Day8 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "1")
    assertEquals(score2, "10")
