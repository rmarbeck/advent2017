// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day13 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "2164")
    assertEquals(score2, "3861798")

  test("Day13 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "24")
    assertEquals(score2, "10")
