sun_pos <- function(hr, doy, timezone, lat, lon) {
   # Calculate the solar position based on time and location
   frac_yr <- 2 * pi / 365 * (doy-1 + hr/24)
   
    # Equation of time (minutes)
    eqtime <- 229.18 * (0.000075
                        + 0.001868 * cos(frac_yr)
                        - 0.032077 * sin(frac_yr)
                        - 0.014615 * cos(2 * frac_yr)
                        - 0.040849 * sin(2 * frac_yr))

    # Solar declination (radians)
    decl <- (0.006918
            - 0.399912 * cos(frac_yr)
            + 0.070257 * sin(frac_yr)
            - 0.006758 * cos(2 * frac_yr)
            + 0.000907 * sin(2 * frac_yr)
            - 0.002697 * cos(3 * frac_yr)
            + 0.001480 * sin(3 * frac_yr))

    # Time offset (minutes)
    time_offset <- eqtime + 4*lon + timezone*60

    # True solar time (minutes)
    tst <- hr %% 24 * 60 + time_offset

    # Solar hour angle (radians)
    ha <- (tst / 4 - 180) * pi / 180

    # Solar zenith angle (radians)
    solar_zenith <- acos(sin(lat * pi / 180) * sin(decl) + cos(lat * pi / 180) * cos(decl) * cos(ha))

    # Solar azimuth angle (radians)
    solar_azimuth <- acos((sin(decl) - sin(lat*pi/180)*cos(solar_zenith)) /
                          (cos(lat*pi/180) * sin(solar_zenith)))
    solar_azimuth <- ifelse(ha > 0, 2*pi - solar_azimuth, solar_azimuth)

    list(zenith = solar_zenith, azimuth = solar_azimuth)
 }

 # --- Function 1: clear-sky global horizontal irradiance (flat ground) ---
# Depends only on how much atmosphere the beam crosses (zenith) + orbit (doy).
clearsky_ghi <- function(zenith, doy) {
  # zenith in radians. Returns clear-sky GHI on flat ground (W/m^2).
  E0    <- 1 + 0.033 * cos(2*pi * doy / 365)      # eccentricity correction
  I_0n  <- 1361 * E0                              # extraterrestrial normal
  cos_z <- cos(zenith)
  elev_deg <- 90 - zenith * 180/pi
  AM <- ifelse(elev_deg > 0,
               1 / (cos_z + 0.50572 * (96.07995 - zenith*180/pi)^(-1.6364)),
               Inf)                               # night -> Inf air mass
  ghi <- I_0n * pmax(cos_z, 0) * 0.7^(AM^0.678)   # transmittance model
  ghi[!is.finite(ghi)] <- 0
  pmax(ghi, 0)
}

erbs_diffuse_frac <- function(ghi, zenith, doy) {
  E0    <- 1 + 0.033 * cos(2*pi * doy / 365)
  I_0n  <- 1361 * E0
  I_0h  <- I_0n * pmax(cos(zenith), 0)              # extraterrestrial horizontal
  kt    <- ifelse(I_0h > 0, ghi / I_0h, 0)          # clearness index
  kt    <- pmin(pmax(kt, 0), 1)                     # clamp to [0,1]

  df <- ifelse(kt <= 0.22,
               1.0 - 0.09 * kt,
        ifelse(kt <= 0.80,
               0.9511 - 0.1604*kt + 4.388*kt^2 - 16.638*kt^3 + 12.336*kt^4,
               0.165))
  pmin(pmax(df, 0), 1)                              # keep in [0,1]
}

# --- Function 2: horizontal irradiance -> irradiance on an oriented wall ---
# Takes ANY horizontal irradiance (clear-sky from fn 1, OR measured GHI).
# wall_az: 0 = north, pi = south (radians).
wall_irradiance <- function(ghi, azimuth, zenith, doy, wall_az, albedo = 0.2) {
  diffuse_frac = erbs_diffuse_frac(ghi, zenith, doy)
  cos_z <- cos(zenith)
  elev  <- pi/2 - zenith

  # split horizontal irradiance into beam + diffuse.
  # diffuse_frac is a rough constant; a proper model (Erbs/DISC) would make it
  # depend on clearness. For measured GHI in cloudy conditions, raise it.
  ghi   <- pmax(ghi, 0)
  dhi   <- ghi * diffuse_frac                     # diffuse horizontal
  bhi   <- ghi - dhi                              # beam horizontal
  bn    <- ifelse(cos_z > 0.01, bhi / cos_z, 0)   # beam normal (guard low sun)

  # --- beam onto vertical wall: angle of incidence ---
  cos_inc <- sin(zenith) * cos(azimuth - wall_az)
  cos_inc <- pmax(cos_inc, 0)                      # 0 when sun behind wall
  beam_wall <- bn * cos_inc

  # --- diffuse: isotropic sky, vertical wall sees half the dome ---
  diff_wall <- dhi * 0.5

  # --- ground-reflected onto vertical wall (view factor 0.5) ---
  refl_wall <- ghi * albedo * 0.5

  total <- ifelse(elev > 0, beam_wall + diff_wall + refl_wall, 0)
  pmax(total, 0)
}

sensor_irradiance <- function(ghi, hr, doy, timezone, lat, lon, wall_az, albedo = 0.2) {
  pos <- sun_pos(hr, doy, timezone, lat, lon)
  wall_irradiance(ghi, pos$azimuth, pos$zenith, doy, wall_az, albedo = albedo) 
}

add_solar <- function(dat, doy_start = 185, timezone = 0,
                      lat = 64.1355, lon = -21.8954, wall_az = 5.17) {
  dat$G <- sensor_irradiance(dat$G, dat$t, doy_start, timezone, lat, lon, wall_az)
  saveRDS(dat, "processed_data/preprocessed_raw/temp_log_33d_solar_geometric.rds")
  dat                                          # return the frame
}