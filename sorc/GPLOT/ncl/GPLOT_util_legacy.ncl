; NCL LEGACY FUNCTIONS FOR GPLOT
; Do not load these unless you have good reason to do so.
; The preferred procedure is to find the function you would like
; to restore and move that into an existing or new scripr that
; can then be loaded by NCL.

;
;================================================================
; findVarName
; Moved To Legacy: December 18 2019

undef("findVarName")
function findVarName (dSource:string,var[1]:string,lev[1]:string)
local oldVar, newVar, masterList, lev2, catNum, levch
begin

        levch   = tochar(lev)
        if(lev.ne."" .and. dimsizes(levch).le.5)then            lev2 = tostring(toint(lev))
        else if(lev.ne."" .and. dimsizes(levch).gt.5)then       lev2 = tostring(toint(tostring(levch(:3))))
        else                                                    lev2 = ""
        end if  end if

        masterList      = (/(/"T",      "101"/),\       ; 101.  3D Air Temperature
                            (/"HGT",    "102"/),\       ; 102.  3D Geopotential Height
                            (/"U",      "103"/),\       ; 103.  3D Zonal Wind
                            (/"V",      "104"/),\       ; 104.  3D Meridional Wind
                            (/"UV",     "105"/),\       ; 105.  3D Total Wind
                            (/"TH",     "106"/),\       ; 106.  3D Potential Temperature
                            (/"RH",     "107"/),\       ; 107.  3D Relative Humidity
                            (/"SPH",    "108"/),\       ; 108.  3D Specific Humidity
                            (/"W",      "109"/),\       ; 109.  3D Vertical Velocity
                            (/"AVO",    "110"/),\       ; 110.  3D Vorticity (absolute)
                            (/"RVO",    "111"/),\       ; 111.  3D Vorticity (relative)
                            (/"DPT",    "112"/),\       ; 112.  3D Dewpoint Temperature
                            (/"U10",    "201"/),\       ; 201.  10-m Zonal Wind
                            (/"V10",    "202"/),\       ; 202.  10-m Meridional Wind
                            (/"UV10",   "203"/),\       ; 203.  10-m Total Wind
                            (/"T2",     "301"/),\       ; 301.  2-m Air Temperature
                            (/"DPT2",   "302"/),\       ; 302.  2-m Dew Point Temperature
                            (/"SPH2",   "303"/),\       ; 303.  2-m Specific Humidity
                            (/"RH2",    "304"/),\       ; 304.  2-m Relative Humidity
                            (/"MSLP",   "401"/),\       ; 401.  Mean Sea Level Pressure
                            (/"LHFLX",  "402"/),\       ; 402.  Surface Latent Heat Flux
                            (/"SHFLX",  "403"/),\       ; 403.  Surface Sensible Heat Flux
                            (/"TSFC",   "404"/),\       ; 404.  Surface Temperature
                            (/"SST",    "405"/),\       ; 405.  Sea Surface Temperature
                            (/"PSFC",   "406"/),\       ; 406.  Surface Pressure
                            (/"TTROP",  "501"/),\       ; 501.  Tropopause Temperature
                            (/"PRCP",   "601"/),\       ; 601.  Bulk Precipitation Rate
                            (/"PRATE",  "602"/),\       ; 602.  Instantaneous Precipitation Rate
                            (/"TPRCP",  "603"/),\       ; 603.  Total Precipitation
                            (/"TPW",    "604"/),\       ; 604.  Total Precipitable Water
                            (/"REFL",   "605"/),\       ; 605.  Composite Reflectivity
                            (/"HLCY",   "606"/),\       ; 606.  Storm-Relative Helicity
                            (/"CAPE",   "607"/),\       ; 607.  Convective Available Potential Energy
                            (/"REFD",   "608"/),\       ; 608.  Reflectivity  (200-100000pa) ; MKadd
                            (/"LAND",  "1001"/)\        ; 1001. Land-Sea Mask
                            /)





        ; If the level is non-pressure, we need to add it to the variable name
        ; Otherwise, oldVar = var
        ; MKnote - UV lev=10 -> U10,V10 ; T lev=2 -> T2
        if(any((/"10","2"/).eq.lev))then
                oldVar = var+lev
        else
                oldVar = var
        end if
        if(.not.any(masterList(:,0).eq.oldVar))then     print("ERROR: findVarName: Variable '"+oldVar+"' not recognized")
                                                        exit
        end if
        catNum  = masterList(ind(masterList(:,0).eq.oldVar),1)

        ; VARIABLE TABLE FOR:  GLOBAL FORECAST SYSTEM
        if(any((/"GFS","GFS_an","GFS_fcst","GFS_d03"/).eq.dSource))then
                varList = (/(/"TMP_P0_L100_GLL0",       "101"/),\       ; 101.  3D Air Temperature
                            (/"HGT_P0_L100_GLL0",       "102"/),\       ; 102.  3D Geopotential Height
                            (/"UGRD_P0_L100_GLL0",      "103"/),\       ; 103.  3D Zonal Wind
                            (/"VGRD_P0_L100_GLL0",      "104"/),\       ; 104.  3D Meridional Wind
                            (/"UGRD_P0_L100_GLL0",      "105"/),\       ; 105.  3D Total Wind
                            (/"RH_P0_L100_GLL0",        "107"/),\       ; 107.  3D Relative Humidity
                            (/"SPFH_P0_L100_GLL0",      "108"/),\       ; 108.  3D Specific Humidity
                            (/"VVEL_P0_L100_GLL0",      "109"/),\       ; 109.  3D Vertical Velocity
                            (/"ABSV_P0_L100_GLL0",      "110"/),\       ; 110.  3D Vorticity (absolute)
                            (/"ABSV_P0_L100_GLL0",      "111"/),\       ; 111.  3D Vorticity (relative)
                            (/"DPT_P0_L100_GLL0",       "112"/),\       ; 112.  3D Dewpoint Temperature
                            (/"UGRD_P0_L103_GLL0",      "201"/),\       ; 201.  10-m Zonal Wind
                            (/"VGRD_P0_L103_GLL0",      "202"/),\       ; 202.  10-m Meridional Wind
                            (/"UGRD_P0_L103_GLL0",      "203"/),\       ; 203.  10-m Total Wind
                            (/"TMP_P0_L103_GLL0",       "301"/),\       ; 301.  2-m Air Temperature
                            (/"DPT_P0_L103_GLL0",       "302"/),\       ; 302.  2-m Dew Point Temperature
                            (/"SPFH_P0_L103_GLL0",      "303"/),\       ; 303.  2-m Specific Humidity
                            (/"RH_P0_L103_GLL0",        "304"/),\       ; 304.  2-m Relative Humidity
                            (/"MSLET_P0_L101_GLL0",     "401"/),\       ; 401.  Mean Sea Level Pressure
                            (/"LHTFL_P0_L1_GLL0",       "402"/),\       ; 402.  Surface Latent Heat Flux
                            (/"SHTFL_P0_L1_GLL0",       "403"/),\       ; 403.  Surface Sensible Heat Flux
                            (/"TMP_P0_L1_GLL0",         "404"/),\       ; 404.  Surface Temperature
                            (/"WTMP_P0_L1_GLL0",        "405"/),\       ; 405.  Sea Surface Temperature
                            (/"PRES_P0_L1_GLL0",        "406"/),\       ; 406.  Surface Pressure
                            (/"TMP_P0_L7_GLL0",         "501"/),\       ; 501.  Tropopause Temperature
                            (/"APCP_P8_L1_GLL0_acc",    "601"/),\       ; 601.  Bulk Precipitation Rate
                            (/"PRATE_P0_L1_GLL0",       "602"/),\       ; 602.  Instantaneous Precipitation Rate
                            (/"APCP_P8_L1_GLL0_acc",    "603"/),\       ; 603.  Total Precipitation
                            (/"PWAT_P0_L200_GLL0",      "604"/),\       ; 604.  Total Precipitable Water
                            (/"REFC_P0_L200_GLL0",      "605"/),\       ; 605.  Composite Reflectivity
                            (/"HLCY_P0_2L103_GLL0",     "606"/),\       ; 606.  Storm-Relative Helicity
                            (/"CAPE_P0_L1_GLL0",        "607"/),\       ; 607.  Convective Available Potential Energy
                            (/"LAND_P0_L1_GLL0",       "1001"/)\        ; 1001. Land-Sea Mask
                            /)

        ; VARIABLE TABLE FOR:  HURRICANE WEATHER RESEARCH AND FORECASTING (HWRF) MODEL
        else if(isStrSubset(dSource,"HWRF"))then
                varList = (/    (/"TMP_P0_L100_GLL0",   "101"/),\       ; 101.  3D Air Temperature
                                (/"HGT_P0_L100_GLL0",   "102"/),\       ; 102.  3D Geopotential Height
                                (/"UGRD_P0_L100_GLL0",  "103"/),\       ; 103.  3D Zonal Wind
                                (/"VGRD_P0_L100_GLL0",  "104"/),\       ; 104.  3D Meridional Wind
                                (/"UGRD_P0_L100_GLL0",  "105"/),\       ; 105.  3D Total Wind
                                (/"RH_P0_L100_GLL0",    "107"/),\       ; 107.  3D Relative Humidity
                                (/"SPFH_P0_L100_GLL0",  "108"/),\       ; 108.  3D Specific Humidity
                                (/"VVEL_P0_L100_GLL0",  "109"/),\       ; 109.  3D Vertical Velocity
                                (/"ABSV_P0_L100_GLL0",  "110"/),\       ; 110.  3D Vorticity (absolute)
                                (/"ABSV_P0_L100_GLL0",  "111"/),\       ; 111.  3D Vorticity (relative)
                                (/"DPT_P0_L100_GLL0",   "112"/),\       ; 112.  3D Dewpoint Temperature
                                (/"UGRD_P0_L103_GLL0",  "201"/),\       ; 201.  10-m Zonal Wind
                                (/"VGRD_P0_L103_GLL0",  "202"/),\       ; 202.  10-m Meridional Wind
                                (/"UGRD_P0_L103_GLL0",  "203"/),\       ; 203.  10-m Total Wind
                                (/"TMP_P0_L103_GLL0",   "301"/),\       ; 301.  2-m Air Temperature
                                (/"DPT_P0_L103_GLL0",   "302"/),\       ; 302.  2-m Dew Point Temperature
                                (/"SPFH_P0_L103_GLL0",  "303"/),\       ; 303.  2-m Specific Humidity
                                (/"RH_P0_L103_GLL0",    "304"/),\       ; 304.  2-m Relative Humidity
                                (/"PRMSL_P0_L101_GLL0", "401"/),\       ; 401.  Mean Sea Level Pressure
                                (/"LHTFL_P0_L1_GLL0",   "402"/),\       ; 402.  Surface Latent Heat Flux
                                (/"SHTFL_P0_L1_GLL0",   "403"/),\       ; 403.  Surface Sensible Heat Flux
                                (/"TMP_P0_L1_GLL0",     "404"/),\       ; 404.  Surface Temperature
                                (/"WTMP_P0_L1_GLL0",    "405"/),\       ; 405.  Sea Surface Temperature
                                (/"PRES_P0_L1_GLL0",    "406"/),\       ; 406.  Surface Pressure
                                (/"TMP_P0_L7_GLL0",     "501"/),\       ; 501.  Tropopause Temperature
                                (/"APCP_P8_L1_GLL0_acc","601"/),\       ; 601.  Bulk Precipitation Rate
                                (/"PRATE_P0_L1_GLL0",   "602"/),\       ; 602.  Instantaneous Precipitation Rate
                                (/"APCP_P8_L1_GLL0_acc","603"/),\       ; 603.  Total Precipitation
                                (/"PWAT_P0_L200_GLL0",  "604"/),\       ; 604.  Total Precipitable Water
                                (/"REFC_P0_L200_GLL0",  "605"/),\       ; 605.  Composite Reflectivity
                                (/"HLCY_P0_2L103_GLL0", "606"/),\       ; 606.  Storm-Relative Helicity
                                (/"CAPE_P0_L1_GLL0",    "607"/),\       ; 607.  Convective Available Potential Energy
                                (/"REFD_P0_L100_GLL0",  "608"/),\       ; 608.  Reflectivity  ;MK
                                (/"LAND_P0_L1_GLL0",    "1001"/)\       ; 1001. Land-Sea Mask
                            /)

        ; VARIABLE TABLE FOR: HURRICANE ANALYSIS AND FORECAST SYSTEM (HAFS)
        else if(isStrSubset(dSource,"HAFS"))then
                varList = (/    (/"TMP_P0_L100_GLL0",   "101"/),\       ; 101.  3D Air Temperature
                                (/"HGT_P0_L100_GLL0",   "102"/),\       ; 102.  3D Geopotential Height
                                (/"UGRD_P0_L100_GLL0",  "103"/),\       ; 103.  3D Zonal Wind
                                (/"VGRD_P0_L100_GLL0",  "104"/),\       ; 104.  3D Meridional Wind
                                (/"UGRD_P0_L100_GLL0",  "105"/),\       ; 105.  3D Total Wind
                                (/"RH_P0_L100_GLL0",    "107"/),\       ; 107.  3D Relative Humidity
                                (/"SPFH_P0_L100_GLL0",  "108"/),\       ; 108.  3D Specific Humidity
                                (/"VVEL_P0_L100_GLL0",  "109"/),\       ; 109.  3D Vertical Velocity
                                (/"ABSV_P0_L100_GLL0",  "110"/),\       ; 110.  3D Vorticity (absolute)
                                (/"ABSV_P0_L100_GLL0",  "111"/),\       ; 111.  3D Vorticity (relative)
                                (/"DPT_P0_L100_GLL0",   "112"/),\       ; 112.  3D Dewpoint Temperature
                                (/"UGRD_P0_L103_GLL0",  "201"/),\       ; 201.  10-m Zonal Wind
                                (/"VGRD_P0_L103_GLL0",  "202"/),\       ; 202.  10-m Meridional Wind
                                (/"UGRD_P0_L103_GLL0",  "203"/),\       ; 203.  10-m Total Wind
                                (/"TMP_P0_L103_GLL0",   "301"/),\       ; 301.  2-m Air Temperature
                                (/"DPT_P0_L103_GLL0",   "302"/),\       ; 302.  2-m Dew Point Temperature
                                (/"SPFH_P0_L103_GLL0",  "303"/),\       ; 303.  2-m Specific Humidity
                                (/"RH_P0_L103_GLL0",    "304"/),\       ; 304.  2-m Relative Humidity
                                (/"MSLET_P0_L101_GLL0", "401"/),\       ; 401.  Mean Sea Level Pressure
                                (/"LHTFL_P0_L1_GLL0",   "402"/),\       ; 402.  Surface Latent Heat Flux
                                (/"SHTFL_P0_L1_GLL0",   "403"/),\       ; 403.  Surface Sensible Heat Flux
                                (/"TMP_P0_L1_GLL0",     "404"/),\       ; 404.  Surface Temperature
                                (/"WTMP_P0_L1_GLL0",    "405"/),\       ; 405.  Sea Surface Temperature
                                (/"PRES_P0_L1_GLL0",    "406"/),\       ; 406.  Surface Pressure
                                (/"TMP_P0_L7_GLL0",     "501"/),\       ; 501.  Tropopause Temperature
                                (/"APCP_P8_L1_GLL0_acc","601"/),\       ; 601.  Bulk Precipitation Rate
                                (/"PRATE_P0_L1_GLL0",   "602"/),\       ; 602.  Instantaneous Precipitation Rate
                                (/"APCP_P8_L1_GLL0_acc","603"/),\       ; 603.  Total Precipitation
                                (/"PWAT_P0_L200_GLL0",  "604"/),\       ; 604.  Total Precipitable Water
                                (/"REFC_P0_L200_GLL0",  "605"/),\       ; 605.  Composite Reflectivity
                                (/"HLCY_P0_2L103_GLL0", "606"/),\       ; 606.  Storm-Relative Helicity
                                (/"CAPE_P0_L1_GLL0",    "607"/),\       ; 607.  Convective Available Potential Energy
                                (/"REFD_P0_L100_GLL0",  "608"/),\       ; 608.  Reflectivity  ;MK
                                (/"LAND_P0_L1_GLL0",    "1001"/)\       ; 1001. Land-Sea Mask
                            /)

        ; VARIABLE TABLE FOR: ERA-INTERIM
        else if(dSource.eq."ERAi")then
                varList = (/(/"TMP_P0_L100_GLL0",       "101"/),\       ; 101.  3D Air Temperature
                            (/"Z_GDS0_ISBL",            "102"/),\       ; 102.  3D Geopotential Height
                            (/"VGRD_P0_L100_GLL0",      "104"/),\       ; 104.  3D Meridional Wind
                            (/"RH_P0_L100_GLL0",        "107"/),\       ; 107.  3D Relative Humidity
                            (/"VVEL_P0_L100_GLL0",      "109"/),\       ; 109.  3D Vertical Velocity
                            (/"ABSV_P0_L100_GLL0",      "110"/),\       ; 110.  3D Vorticity (absolute)
                            (/"ABSV_P0_L100_GLL0",      "111"/),\       ; 111.  3D Vorticity (relative)
                            (/"UGRD_P0_L100_GLL0",      "103"/),\       ; 103.  3D Zonal Wind
                            (/"UGRD_P0_L100_GLL0",      "105"/)\        ; 105.  3D Total Wind
                            /)

        ; VARIABLE TABLE FOR: 2013 BASIN-SCALE HWRF
        else if(any((/"H3HW","HEDAS"/).eq.dSource))then
                varList = (/    (/"TMP_GDS0_ISBL",      "101"/),\       ; 101.  3D Air Temperature
                                (/"HGT_GDS0_ISBL",      "102"/),\       ; 102.  3D Geopotential Height
                                (/"U_GRD_GDS0_ISBL",    "103"/),\       ; 103.  3D Zonal Wind
                                (/"V_GRD_GDS0_ISBL",    "104"/),\       ; 104.  3D Meridional Wind
                                (/"U_GRD_GDS0_ISBL",    "105"/),\       ; 105.  3D Total Wind
                                (/"R_H_GDS0_ISBL",      "107"/),\       ; 107.  3D Relative Humidity
                                (/"SPF_H_GDS0_ISBL",    "108"/),\       ; 108.  3D Specific Humidity
                                (/"V_VEL_GDS0_ISBL",    "109"/),\       ; 109.  3D Vertical Velocity
                                (/"ABS_V_GDS0_ISBL",    "110"/),\       ; 110.  3D Vorticity (absolute)
                                (/"ABS_V_GDS0_ISBL",    "111"/),\       ; 111.  3D Vorticity (relative)
                                (/"P_WAT_GDS0_EATM",    "604"/),\       ; 604.  Total Precipitable Water
                                (/"REFC_GDS0_EATM",     "605"/)\        ; 605.  Composite Radar Reflectivity
                            /)

        ; VARIABLE TABLE FOR:  FVGFS_ATL
        else if(isStrSubset(dSource,"fvGFS_ATL"))then
                varList = (/(/"TMP"+lev2,               "101"/),\       ; 101.  3D Air Temperature
                            (/"HGT"+lev2,               "102"/),\       ; 102.  3D Geopotential Height
                            (/"VGRD"+lev2,              "104"/),\       ; 104.  3D Meridional Wind
                            (/"RH"+lev2,                "107"/),\       ; 107.  3D Relative Humidity
                            (/"VVEL"+lev2,              "109"/),\       ; 109.  3D Vertical Velocity
                            (/"UGRD"+lev2,              "103"/),\       ; 103.  3D Zonal Wind
                            (/"PRMSL",                  "401"/),\       ; 401.  Mean Sea Level Pressure
                            (/"UGRD"+lev2,              "105"/),\       ; 105.  3D Total Wind
                            (/"UGRD"+lev2,              "110"/),\       ; 110.  3D Vorticity (absolute)
                            (/"UGRD"+lev2,              "111"/),\       ; 111.  3D Vorticity (relative)
                            (/"PRESsfc",                "406"/),\       ; 406.  Surface Pressure
                            (/"PWAT",                   "604"/),\       ; 604.  Total Precipitable Water
                            (/"REFC",                   "605"/),\       ; 605.  Composite Reflectivity
                            (/"CAPEsfc",                "607"/),\       ; 607.  Convective Available Potential Energy
                            (/"UGRD10m",                "201"/),\       ; 201.  10-m Zonal Wind
                            (/"VGRD10m",                "202"/),\       ; 202.  10-m Meridional Wind
                            (/"UGRD10m",                "203"/),\       ; 203.  10-m Total Wind
                            (/"TMP2m",                  "301"/),\       ; 301.  2-m Air Temperature
                            (/"TMPsfc",                 "404"/),\       ; 404.  Surface Temperature
                            (/"DPT2m",                  "302"/),\       ; 302.  2-m Dew Point Temperature
                            (/"DPT"+lev2,               "112"/),\       ; 112.  3D Dewpoint Temperature
                            (/"SPFH2m",                 "303"/),\       ; 303.  2-m Specific Humidity
                            (/"PRATEsfc",               "602"/)\        ; 602.  Instantaneous Precipitation Rate
                            /)

        ; VARIABLE TABLE FOR:  GEFS
        else if(any((/"GEFS"/).eq.dSource))then
                varList = (/(/"TMP_P1_L100_GLL0",       "101"/),\       ; 101.  3D Air Temperature
                            (/"HGT_P1_L100_GLL0",       "102"/),\       ; 102.  3D Geopotential Height
                            (/"UGRD_P1_L100_GLL0",      "103"/),\       ; 103.  3D Zonal Wind
                            (/"VGRD_P1_L100_GLL0",      "104"/),\       ; 104.  3D Meridional Wind
                            (/"UGRD_P1_L100_GLL0",      "105"/),\       ; 105.  3D Total Wind
                            (/"RH_P1_L100_GLL0",        "107"/),\       ; 107.  3D Relative Humidity
                            (/"SPFH_P0_L100_GLL0",      "108"/),\       ; 108.  3D Specific Humidity
                            (/"ABSV_P0_L100_GLL0",      "110"/),\       ; 110.  3D Vorticity (absolute)
                            (/"ABSV_P0_L100_GLL0",      "111"/),\       ; 111.  3D Vorticity (relative)
                            (/"DPT_P0_L100_GLL0",       "112"/),\       ; 112.  3D Dewpoint Temperature
                            (/"UGRD_P1_L103_GLL0",      "201"/),\       ; 201.  10-m Zonal Wind
                            (/"VGRD_P1_L103_GLL0",      "202"/),\       ; 202.  10-m Meridional Wind
                            (/"UGRD_P0_L103_GLL0",      "203"/),\       ; 203.  10-m Total Wind
                            (/"TMP_P1_L103_GLL0",       "301"/),\       ; 301.  2-m Air Temperature
                            (/"DPT_P0_L103_GLL0",       "302"/),\       ; 302.  2-m Dew Point Temperature
                            (/"SPFH_P0_L103_GLL0",      "303"/),\       ; 303.  2-m Specific Humidity
                            (/"RH_P1_L103_GLL0",        "304"/),\       ; 304.  2-m Relative Humidity
                            (/"PRMSL_P1_L101_GLL0",     "401"/),\       ; 401.  Mean Sea Level Pressure
                            (/"LHTFL_P11_L1_GLL0_avg",  "402"/),\       ; 402.  Surface Latent Heat Flux
                            (/"SHTFL_P11_L1_GLL0_avg",  "403"/),\       ; 403.  Surface Sensible Heat Flux
                            (/"WTMP_P0_L1_GLL0",        "405"/),\       ; 405.  Sea Surface Temperature
                            (/"PRES_P1_L1_GLL0",        "406"/),\       ; 406.  Surface Pressure
                            (/"HGT_P1_L1_GLL0",         "407"/),\       ; 407.  Surface Geopotential Height
                            (/"TMP_P0_L7_GLL0",         "501"/),\       ; 501.  Tropopause Temperature
                            (/"ULWRF_P11_L8_GLL0_avg",  "502"/),\       ; 502.  Upward Long-Wave Radiation Flux (TOA)
                            (/"APCP_P8_L1_GLL0_acc",    "601"/),\       ; 601.  Bulk Precipitation Rate
                            (/"PRATE_P0_L1_GLL0",       "602"/),\       ; 602.  Instantaneous Precipitation Rate
                            (/"APCP_P8_L1_GLL0_acc",    "603"/),\       ; 603.  Total Precipitation
                            (/"PWAT_P1_L200_GLL0",      "604"/),\       ; 604.  Total Precipitable Water
                            (/"REFC_P0_L200_GLL0",      "605"/),\       ; 605.  Composite Reflectivity
                            (/"HLCY_P0_2L103_GLL0",     "606"/),\       ; 606.  Storm-Relative Helicity
                            (/"CAPE_P1_2L108_GLL0",     "607"/),\       ; 607.  Convective Available Potential Energy (Surface to 180 hPa)
                            (/"CIN_P1_2L108_GLL0",      "609"/),\       ; 607.  Convective Inhibition (Surface to 180 hPa)
                            (/"VVEL_P1_L100_GLL0",      "701"/),\       ; 701.  850 hPa Vertical Velocity
                            (/"LAND_P0_L1_GLL0",       "1001"/)\        ; 1001. Land-Sea Mask
                            /)

        else if(any((/"ECMWF"/).eq.dSource))then
                varList = (/(/"TMP_P0_L100_GLL0",       "101"/),\       ; 101.  3D Air Temperature
                            (/"HGT_P0_L100_GLL0",       "102"/),\       ; 102.  3D Geopotential Height
                            (/"UGRD_P0_L100_GLL0",      "103"/),\       ; 103.  3D Zonal Wind
                            (/"VGRD_P0_L100_GLL0",      "104"/),\       ; 104.  3D Meridional Wind
                            (/"UGRD_P0_L100_GLL0",      "105"/),\       ; 105.  3D Total Wind
                            (/"PRES_P0_L101_GLL0",      "401"/)\        ; 401.  Mean Sea Level Pressure
                            /)

        end if  end if  end if  end if  end if  end if  end if  end if

        match   = ind(varList(:,1).eq.catNum)
        if(ismissing(match(0)))then     print("WARNING: findVarName: Variable '"+oldVar+"' not found in table for "+dSource+".")
                                        newVar = "missing"
                                        return(newVar)
        else                            newVar  = varList(match,0)
        end if


        ; This is a temp. fix for Precipitation variables that were updated to have time interval information.
        ; This should be integrated into the actual variable name, as with UV10, T2, etc.
        if(any((/"GFS","HAFS"/).eq.dSource))then
                if(isStrSubset(var,"PRCP") .and. isStrSubset(lev,"h"))then      newVar = newVar+lev   end if
        end if


        return(newVar)

end

