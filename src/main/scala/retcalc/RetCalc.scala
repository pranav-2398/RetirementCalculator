package retcalc

import retcalc.RetCalcError.MoreExpensesthanIncome

import scala.annotation.tailrec

case class RetCalcParams(
                          nbOfMonthsInRetirement: Int,
                          netIncome: Int,
                          currentExpenses: Int,
                          initialCapital: Double
                        )

object RetCalc {
  def futureCapital(
                     returns: Returns, nbOfMonths: Int, netIncome: Int,
                     currentExpenses: Int, initialCapital: Double
                   ): Either[RetCalcError, Double] = {
    val monthlySavings = netIncome - currentExpenses

    (0 until nbOfMonths).foldLeft[Either[RetCalcError, Double]](Right(initialCapital)) {
      case (accumulated, month) =>
        for {
          acc <- accumulated
          monthlyRate <- Returns.monthlyRate(returns, month)
        } yield acc * (1 + monthlyRate) + monthlySavings
    }
  }

  def simulatePlan(
                    returns: Returns, params: RetCalcParams, nbOfMonthsSavings: Int,
                    monthOffset: Int = 0
                  ): Either[RetCalcError, (Double, Double)] = {
    import params._

    for {
      capitalAtRetirement <- futureCapital(
        returns = returns,
        nbOfMonths = nbOfMonthsSavings,
        netIncome = netIncome,
        currentExpenses = currentExpenses,
        initialCapital = initialCapital
      )

      capitalAfterDeath <- futureCapital(
        returns = OffsetReturns(returns, monthOffset + nbOfMonthsSavings),
        nbOfMonths = nbOfMonthsInRetirement,
        netIncome = 0,
        currentExpenses = currentExpenses,
        initialCapital = capitalAtRetirement
      )
    } yield (capitalAtRetirement, capitalAfterDeath)
  }

  def nbOfMonthsSaving(
                        params: RetCalcParams, returns: Returns
                      ): Either[RetCalcError, Int] = {
    import params._
    @tailrec
    def loop(months: Int): Either[RetCalcError, Int] = {
      simulatePlan(returns, params, months) match {
        case Right((capitalAtRetirement, capitalAfterDeath)) =>
          if (capitalAfterDeath > 0.0)
            Right(months)
          else
            loop(months + 1)

        case Left(err) => Left(err)
      }
    }

    if (netIncome > currentExpenses)
      loop(0)
    else
      Left(MoreExpensesthanIncome(netIncome, currentExpenses))
  }
}