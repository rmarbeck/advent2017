// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day17 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "1914")
    assertEquals(score2, "41797835")

  test("Day17 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "638")
    assertEquals(score2, "1222153")
