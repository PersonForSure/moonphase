import MoonPhase (moonPhase)

illumFrac :: Double -> Double
illumFrac d = ((1.0 - cos d) / 2.0) * 100.0

main :: IO ()
main = do
  let pass =
        and
          [ abs (illumFrac (moonPhase (-178070400.0)) - 1.2) < 0.05,
            abs (illumFrac (moonPhase 361411200.0) - 93.6) < 0.05,
            abs (illumFrac (moonPhase 1704931200.0) - 0.4) < 0.05,
            abs (illumFrac (moonPhase 2898374400.0) - 44.2) < 0.05
          ]
  putStrLn $ if pass then "TESTS PASSED" else "TESTS FAILED"
