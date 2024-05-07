// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day19 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "GPALMJSOY")
    assertEquals(score2, "16204")

  test("Day19 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "ABCDEF")
    assertEquals(score2, "38")
