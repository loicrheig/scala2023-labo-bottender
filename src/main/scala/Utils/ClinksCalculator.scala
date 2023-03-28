package Utils

/**
  * Contains the function necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator:
  /**
    * Calculate the factorial of a given number
    * @param n the number to compute
    * @return n!
    */
  // TODO - Part 1 Step 1
  def factorial(n: Int): BigInt = {
    def loop (n: Int, acc: BigInt): BigInt = {
      if (n == 0) acc
      else loop(n - 1, acc * n)
    }
    loop(n, 1)
  }

  /**
    * Calculate the combination of two given numbers
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  // TODO - Part 1 Step 1
  def calculateCombination(n: Int, k: Int): Int = {
    val numerator = factorial(n)
    val denominator = factorial(k) * factorial(n - k)
    (numerator / denominator).toInt
  }
end ClinksCalculator