package sim

import org.scalatest._
import csb._

class SimulationSpec extends FlatSpec with Matchers {

  "RaceRecord" should "print itself correctly" in {

    val recorder = RaceRecord(
      3,
      List(
        Point(5655, -2567),
        Point(4093, -7460),
        Point(13488, -2344),
        Point(12948, -7255)
      ),
      List(
        List(
          Record(
            Pod(
              Point(6078, -2770),
              List(
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255)
              ),
              Point(-0.08214283837133374, -0.9966205667676646),
              Point(-5, -57)
            ),
            Some(
              Move(
                Point(3657.6090240582307, -4526.499598349398),
                138.57104500478908,
                "PILOT1-CORRECTED"
              )
            )
          ),
          Record(
            Pod(
              Point(6922, -3053),
              List(
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255)
              ),
              Point(-0.249215005963018, -0.9684481817850932),
              Point(-17, -65)
            ),
            Some(
              Move(
                Point(2886.228847463513, 1156.3448649671209),
                60,
                "PILOT0-CORRECTED"
              )
            )
          ),
          Record(
            Pod(
              Point(5192, -2487),
              List(
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255)
              ),
              Point(-0.5105084131039128, -0.8598727581160626),
              Point(-29, -49)
            ),
            Some(
              Move(
                Point(10074.239426897095, -1135.9234506289276),
                60,
                "PILOT0-CORRECTED"
              )
            )
          ),
          Record(
            Pod(
              Point(4341, -2230),
              List(
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255)
              ),
              Point(-0.3580156872661533, -0.933715571076837),
              Point(-24, -63)
            ),
            Some(
              Move(
                Point(10074.239426897095, -1135.9234506289276),
                60,
                "PILOT0-CORRECTED"
              )
            )
          )
        ),
        List(
          Record(
            Pod(
              Point(6105, -2962),
              List(
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255)
              ),
              Point(0.2298502103747638, -0.9732260173210932),
              Point(22, -164)
            ),
            Some(
              Move(
                Point(3731.003718664278, -5048.385737229311),
                125.76538187685465,
                "PILOT0-CORRECTED"
              )
            )
          ),
          Record(
            Pod(
              Point(6909, -3178),
              List(
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255)
              ),
              Point(0.06224939096344839, -0.9980606260767326),
              Point(-12, -107)
            ),
            Some(
              Move(
                Point(2692.7863103516893, 1496.4948207019925),
                60,
                "PILOT0-CORRECTED"
              )
            )
          ),
          Record(
            Pod(
              Point(5021, -2677),
              List(
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255)
              ),
              Point(-0.711556089215331, -0.7026292990621612),
              Point(-146, -162)
            ),
            Some(
              Move(
                Point(10440.171798564217, -976.1539805655154),
                60,
                "PILOT0-CORRECTED"
              )
            )
          ),
          Record(
            Pod(
              Point(4279, -2340),
              List(
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255),
                Point(5655, -2567),
                Point(4093, -7460),
                Point(13488, -2344),
                Point(12948, -7255)
              ),
              Point(-0.6290271316856146, -0.7773833466208085),
              Point(-53, -94)
            ),
            Some(
              Move(
                Point(10440.171798564217, -976.1539805655154),
                60,
                "PILOT0-CORRECTED"
              )
            )
          )
        )
      )
    )
  }

  def checkSimulation(recorder: RaceRecord, step: Int) = {
    val race = recorder.step(step)
    val expectecdRace = recorder.step(step + 1)
    val computedRace = race.simulate(recorder.stepMoves(step).flatten)

    def compare(podA: Pod, podB: Pod) = {
      assert(podA.orientation == podB.orientation, "in orientation computation")
      assert(podA.speed == podB.speed, "in speed computation")
      assert(podA.position == podB.position, "in position computation")
    }
    compare(computedRace.pods(0), expectecdRace.pods(0))
    compare(computedRace.pods(1), expectecdRace.pods(1))
  }

  "Simulation" should "compute next position when pod speed is 50 and direction is horizontal" in {

    val recorder = RaceRecord(
      3,
      List(
        Point(11199.0, -5428.0),
        Point(7258.0, -6679.0),
        Point(5402.0, -2828.0),
        Point(10295.0, -3366.0)
      ),
      List(
        List(
          Record(
            Pod(
              Point(11350.0, -5905.0),
              List(Point(7258.0, -6679.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(11450.0, -5905.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(11048.0, -4951.0),
              List(Point(7258.0, -6679.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(11148.0, -4951.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(11653.0, -6858.0),
              List(Point(7258.0, -6679.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10745.0, -3998.0),
              List(Point(7258.0, -6679.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(11400.0, -5905.0),
              List(Point(7258.0, -6679.0)),
              Point(1.0, 0.0),
              Point(42.0, 0.0)
            ),
            Some(Move(Point(11500.0, -5905.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(11098.0, -4951.0),
              List(Point(7258.0, -6679.0)),
              Point(1.0, 0.0),
              Point(42.0, 0.0)
            ),
            Some(Move(Point(11198.0, -4951.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(11573.0, -6855.0),
              List(Point(7258.0, -6679.0)),
              Point(-0.9993908270190958, 0.03489949670250114),
              Point(-67.0, 2.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10682.0, -4047.0),
              List(Point(7258.0, -6679.0)),
              Point(-0.7880107536067219, -0.6156614753256584),
              Point(-53.0, -41.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(11492.0, -5905.0),
              List(Point(7258.0, -6679.0)),
              Point(1.0, 0.0),
              Point(78.0, 0.0)
            ),
            Some(Move(Point(11592.0, -5905.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(11190.0, -4951.0),
              List(Point(7258.0, -6679.0)),
              Point(1.0, 0.0),
              Point(78.0, 0.0)
            ),
            Some(Move(Point(11290.0, -4951.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(11426.0, -6850.0),
              List(Point(7258.0, -6679.0)),
              Point(-0.9993908270190958, 0.03489949670250114),
              Point(-124.0, 4.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10566.0, -4137.0),
              List(Point(7258.0, -6679.0)),
              Point(-0.7880107536067219, -0.6156614753256584),
              Point(-98.0, -76.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(11620.0, -5905.0),
              List(Point(7258.0, -6679.0)),
              Point(1.0, 0.0),
              Point(108.0, 0.0)
            ),
            Some(Move(Point(11720.0, -5905.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(11318.0, -4951.0),
              List(Point(7258.0, -6679.0)),
              Point(1.0, 0.0),
              Point(108.0, 0.0)
            ),
            Some(Move(Point(11418.0, -4951.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(11222.0, -6843.0),
              List(Point(7258.0, -6679.0)),
              Point(-0.9993908270190958, 0.03489949670250114),
              Point(-173.0, 6.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10405.0, -4262.0),
              List(Point(7258.0, -6679.0)),
              Point(-0.7880107536067219, -0.6156614753256584),
              Point(-137.0, -106.0)
            ),
            None
          )
        )
      )
    )

    checkSimulation(recorder, 1)
  }

  it should "compute next position when pod speed is 50 and direction is vertical" in {

    val recorder = RaceRecord(
      3,
      List(
        Point(9115.0, -1866.0),
        Point(4995.0, -5237.0),
        Point(11460.0, -6099.0)
      ),
      List(
        List(
          Record(
            Pod(
              Point(9432.0, -2253.0),
              List(Point(4995.0, -5237.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(9432.0, -2153.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(8798.0, -1479.0),
              List(Point(4995.0, -5237.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(8798.0, -1379.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(10065.0, -3027.0),
              List(Point(4995.0, -5237.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(8165.0, -705.0),
              List(Point(4995.0, -5237.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(9432.0, -2203.0),
              List(Point(4995.0, -5237.0)),
              Point(6.123233995736766e-17, 1.0),
              Point(0.0, 42.0)
            ),
            Some(Move(Point(9432.0, -2103.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(8798.0, -1429.0),
              List(Point(4995.0, -5237.0)),
              Point(6.123233995736766e-17, 1.0),
              Point(0.0, 42.0)
            ),
            Some(Move(Point(8798.0, -1329.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(9992.0, -3059.0),
              List(Point(4995.0, -5237.0)),
              Point(-0.913545457642601, -0.40673664307580004),
              Point(-62.0, -27.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(8119.0, -771.0),
              List(Point(4995.0, -5237.0)),
              Point(-0.5735764363510458, -0.819152044288992),
              Point(-38.0, -55.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(9432.0, -2111.0),
              List(Point(4995.0, -5237.0)),
              Point(6.123233995736766e-17, 1.0),
              Point(0.0, 78.0)
            ),
            Some(Move(Point(9432.0, -2011.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(8798.0, -1337.0),
              List(Point(4995.0, -5237.0)),
              Point(6.123233995736766e-17, 1.0),
              Point(0.0, 78.0)
            ),
            Some(Move(Point(8798.0, -1237.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(9857.0, -3118.0),
              List(Point(4995.0, -5237.0)),
              Point(-0.913545457642601, -0.40673664307580004),
              Point(-115.0, -50.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(8035.0, -892.0),
              List(Point(4995.0, -5237.0)),
              Point(-0.5735764363510458, -0.819152044288992),
              Point(-71.0, -102.0)
            ),
            None
          )
        )
      )
    )

    checkSimulation(recorder, 1)
  }

  it should "compute next position when pod speed is 100 and direction is vertical" in {

    val recorder = RaceRecord(
      3,
      List(
        Point(11294.0, -2842.0),
        Point(7502.0, -6917.0),
        Point(5999.0, -5358.0)
      ),
      List(
        List(
          Record(
            Pod(
              Point(11660.0, -3183.0),
              List(Point(7502.0, -6917.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(11660.0, -3083.0), 100.0, "TEST"))
          ),
          Record(
            Pod(
              Point(10928.0, -2501.0),
              List(Point(7502.0, -6917.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(10928.0, -2401.0), 100.0, "TEST"))
          ),
          Record(
            Pod(
              Point(12392.0, -3864.0),
              List(Point(7502.0, -6917.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10196.0, -1820.0),
              List(Point(7502.0, -6917.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(11660.0, -3083.0),
              List(Point(7502.0, -6917.0)),
              Point(6.123233995736766e-17, 1.0),
              Point(0.0, 85.0)
            ),
            Some(Move(Point(11660.0, -2983.0), 100.0, "TEST"))
          ),
          Record(
            Pod(
              Point(10928.0, -2401.0),
              List(Point(7502.0, -6917.0)),
              Point(6.123233995736766e-17, 1.0),
              Point(0.0, 85.0)
            ),
            Some(Move(Point(10928.0, -2301.0), 100.0, "TEST"))
          ),
          Record(
            Pod(
              Point(12324.0, -3906.0),
              List(Point(7502.0, -6917.0)),
              Point(-0.848048096156426, -0.5299192642332049),
              Point(-57.0, -36.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10159.0, -1891.0),
              List(Point(7502.0, -6917.0)),
              Point(-0.46947156278589053, -0.8829475928589271),
              Point(-31.0, -60.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(11660.0, -2898.0),
              List(Point(7502.0, -6917.0)),
              Point(6.123233995736766e-17, 1.0),
              Point(0.0, 157.0)
            ),
            Some(Move(Point(11660.0, -2798.0), 100.0, "TEST"))
          ),
          Record(
            Pod(
              Point(10928.0, -2216.0),
              List(Point(7502.0, -6917.0)),
              Point(6.123233995736766e-17, 1.0),
              Point(0.0, 157.0)
            ),
            Some(Move(Point(10928.0, -2116.0), 100.0, "TEST"))
          ),
          Record(
            Pod(
              Point(12199.0, -3984.0),
              List(Point(7502.0, -6917.0)),
              Point(-0.848048096156426, -0.5299192642332049),
              Point(-106.0, -66.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10091.0, -2022.0),
              List(Point(7502.0, -6917.0)),
              Point(-0.46947156278589053, -0.8829475928589271),
              Point(-58.0, -111.0)
            ),
            None
          )
        )
      )
    )

    checkSimulation(recorder, 1)
  }

  it should "compute next position when pod speed is 50 and direction is diagonal" in {

    val recorder = RaceRecord(
      3,
      List(
        Point(8699.0, -7436.0),
        Point(7199.0, -2176.0),
        Point(3628.0, -5253.0),
        Point(13818.0, -5068.0),
        Point(10709.0, -2252.0)
      ),
      List(
        List(
          Record(
            Pod(
              Point(8218.0, -7573.0),
              List(Point(7199.0, -2176.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(8118.0, -7673.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(9180.0, -7299.0),
              List(Point(7199.0, -2176.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(9080.0, -7399.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(7257.0, -7847.0),
              List(Point(7199.0, -2176.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10141.0, -7025.0),
              List(Point(7199.0, -2176.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(8183.0, -7608.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.7071067811865475, -0.7071067811865476),
              Point(-30.0, -30.0)
            ),
            Some(Move(Point(8083.0, -7708.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(9145.0, -7334.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.7071067811865475, -0.7071067811865476),
              Point(-30.0, -30.0)
            ),
            Some(Move(Point(9045.0, -7434.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(7256.0, -7767.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.017452406437283255, 0.9998476951563913),
              Point(0.0, 67.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10100.0, -6957.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.5150380749100543, 0.8571673007021123),
              Point(-35.0, 58.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(8118.0, -7673.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.7071067811865475, -0.7071067811865476),
              Point(-55.0, -55.0)
            ),
            Some(Move(Point(8018.0, -7773.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(9080.0, -7399.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.7071067811865475, -0.7071067811865476),
              Point(-55.0, -55.0)
            ),
            Some(Move(Point(8980.0, -7499.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(7255.0, -7620.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.017452406437283255, 0.9998476951563913),
              Point(0.0, 124.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(10024.0, -6831.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.5150380749100543, 0.8571673007021123),
              Point(-65.0, 107.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(8028.0, -7763.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.7071067811865475, -0.7071067811865476),
              Point(-76.0, -76.0)
            ),
            Some(Move(Point(7928.0, -7863.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(8990.0, -7489.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.7071067811865475, -0.7071067811865476),
              Point(-76.0, -76.0)
            ),
            Some(Move(Point(8890.0, -7589.0), 50.0, "TEST"))
          ),
          Record(
            Pod(
              Point(7254.0, -7416.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.017452406437283255, 0.9998476951563913),
              Point(0.0, 173.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(9917.0, -6656.0),
              List(Point(7199.0, -2176.0)),
              Point(-0.5150380749100543, 0.8571673007021123),
              Point(-90.0, 149.0)
            ),
            None
          )
        )
      )
    )
    checkSimulation(recorder, 1)
  }

  it should "compute next position when pod speed is 50 and direction is changing" in {

    val recorder = RaceRecord(
      3,
      List(
        Point(11451.0, -6104.0),
        Point(9075.0, -1844.0),
        Point(5000.0, -5267.0)
      ),
      List(
        List(
          Record(
            Pod(
              Point(11014.0, -6348.0),
              List(Point(9075.0, -1844.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(12014.0, -6348.0), 50.0, "TEST-UP"))
          ),
          Record(
            Pod(
              Point(11888.0, -5860.0),
              List(Point(9075.0, -1844.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            Some(Move(Point(12888.0, -5860.0), 50.0, "TEST-UP"))
          ),
          Record(
            Pod(
              Point(10141.0, -6835.0),
              List(Point(9075.0, -1844.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(12761.0, -5373.0),
              List(Point(9075.0, -1844.0)),
              Point(0.9998476951563913, 0.01745240643728351),
              Point(0.0, 0.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(11064.0, -6348.0),
              List(Point(9075.0, -1844.0)),
              Point(1.0, 0.0),
              Point(42.0, 0.0)
            ),
            Some(Move(Point(11064.0, -5348.0), 50.0, "TEST-LEFT"))
          ),
          Record(
            Pod(
              Point(11938.0, -5860.0),
              List(Point(9075.0, -1844.0)),
              Point(1.0, 0.0),
              Point(42.0, 0.0)
            ),
            Some(Move(Point(11938.0, -4860.0), 50.0, "TEST-LEFT"))
          ),
          Record(
            Pod(
              Point(10124.0, -6757.0),
              List(Point(9075.0, -1844.0)),
              Point(-0.20791169081775956, 0.9781476007338056),
              Point(-14.0, 66.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(12703.0, -5318.0),
              List(Point(9075.0, -1844.0)),
              Point(-0.719339800338651, 0.6946583704589975),
              Point(-49.0, 47.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(11154.0, -6333.0),
              List(Point(9075.0, -1844.0)),
              Point(0.9510565162951535, 0.3090169943749474),
              Point(76.0, 13.0)
            ),
            Some(Move(Point(12154.0, -6333.0), 50.0, "TEST-UP"))
          ),
          Record(
            Pod(
              Point(12028.0, -5845.0),
              List(Point(9075.0, -1844.0)),
              Point(0.9510565162951535, 0.3090169943749474),
              Point(76.0, 13.0)
            ),
            Some(Move(Point(13028.0, -5845.0), 50.0, "TEST-UP"))
          ),
          Record(
            Pod(
              Point(10093.0, -6613.0),
              List(Point(9075.0, -1844.0)),
              Point(-0.20791169081775956, 0.9781476007338056),
              Point(-26.0, 122.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(12596.0, -5216.0),
              List(Point(9075.0, -1844.0)),
              Point(-0.719339800338651, 0.6946583704589975),
              Point(-90.0, 86.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(11280.0, -6320.0),
              List(Point(9075.0, -1844.0)),
              Point(1.0, 0.0),
              Point(107.0, 11.0)
            ),
            Some(Move(Point(11280.0, -5320.0), 50.0, "TEST-LEFT"))
          ),
          Record(
            Pod(
              Point(12154.0, -5832.0),
              List(Point(9075.0, -1844.0)),
              Point(1.0, 0.0),
              Point(107.0, 11.0)
            ),
            Some(Move(Point(12154.0, -4832.0), 50.0, "TEST-LEFT"))
          ),
          Record(
            Pod(
              Point(10050.0, -6413.0),
              List(Point(9075.0, -1844.0)),
              Point(-0.20791169081775956, 0.9781476007338056),
              Point(-36.0, 170.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(12448.0, -5075.0),
              List(Point(9075.0, -1844.0)),
              Point(-0.719339800338651, 0.6946583704589975),
              Point(-125.0, 120.0)
            ),
            None
          )
        ),
        List(
          Record(
            Pod(
              Point(11435.0, -6294.0),
              List(Point(9075.0, -1844.0)),
              Point(0.9510565162951535, 0.3090169943749474),
              Point(131.0, 22.0)
            ),
            Some(Move(Point(12435.0, -6294.0), 50.0, "TEST-UP"))
          ),
          Record(
            Pod(
              Point(12309.0, -5806.0),
              List(Point(9075.0, -1844.0)),
              Point(0.9510565162951535, 0.3090169943749474),
              Point(131.0, 22.0)
            ),
            Some(Move(Point(13309.0, -5806.0), 50.0, "TEST-UP"))
          ),
          Record(
            Pod(
              Point(9997.0, -6165.0),
              List(Point(9075.0, -1844.0)),
              Point(-0.20791169081775956, 0.9781476007338056),
              Point(-44.0, 211.0)
            ),
            None
          ),
          Record(
            Pod(
              Point(12265.0, -4900.0),
              List(Point(9075.0, -1844.0)),
              Point(-0.719339800338651, 0.6946583704589975),
              Point(-155.0, 149.0)
            ),
            None
          )
        )
      )
    )

    checkSimulation(recorder, 2)
  }

}
