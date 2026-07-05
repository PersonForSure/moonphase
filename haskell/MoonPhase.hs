module MoonPhase where

moonPhase :: Double -> Double
moonPhase ud =
  let eccent = 0.016718 -- Eccentricity of Earth's orbit
      elonge = 278.833540 -- Ecliptic longitude of the Sun at epoch 1980.0
      elongp = 282.596403 -- Ecliptic longitude of the Sun at perigee
      torad = pi / 180.0

      fixangle :: Double -> Double
      fixangle a = a - 360 * fromIntegral (floor (a / 360))

      -- Calculation of the Sun's position
      day = (ud / 86400 + 2440587.5) - 2444238.5 -- Date within epoch
      mRad = torad * fixangle (((360 / 365.2422) * day) + elonge - elongp) -- Convert from perigee co-ordinates to epoch 1980.0

      -- Solve equation of Kepler
      solveKepler :: Double -> Double
      solveKepler ePrev =
        let delta = ePrev - eccent * sin ePrev - mRad
            eNext = ePrev - delta / (1 - eccent * cos ePrev)
         in if abs delta <= 1E-6
              then eNext
              else solveKepler eNext

      eInitial = mRad
      eFinal = solveKepler eInitial
      ec = 2 * atan (sqrt ((1 + eccent) / (1 - eccent)) * tan (eFinal / 2)) -- True anomaly
      lambdaSun = fixangle ((ec * (180.0 / pi)) + elongp) -- Sun's geocentric ecliptic longitude
      ml = fixangle (13.1763966 * day + 64.975464) -- Mean longitudde
      mm = fixangle (ml - 0.1114041 * day - 349.383063) -- Mean anomaly
      ev = 1.2739 * sin (torad * (2 * (ml - lambdaSun) - mm)) -- Evection
      ae = 0.1858 * sin mRad -- Annual equation
      mmP = torad * (mm + ev - ae - (0.37 * sin mRad)) -- Corrected anomaly
      lP = ml + ev + (6.2886 * sin mmP) - ae + (0.214 * sin (2 * mmP)) -- Corrected longitude
      lPP = lP + (0.6583 * sin (torad * (2 * (lP - lambdaSun)))) -- True longitude
      moonAgeDeg = lPP - lambdaSun -- Age of the Moon in degrees
   in moonAgeDeg * torad
