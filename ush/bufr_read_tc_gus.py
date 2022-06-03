# (C) Copyright 2005- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities granted to it by
# virtue of its status as an intergovernmental organisation nor does it submit to any jurisdiction.
#
# Python implementation:  bufr_read_tropical_cyclone
#
# Description: how to read data of the ECMWF EPS tropical cyclone tracks encoded in BUFR format.
#
# Please note that tropical cyclone tracks can be encoded in various ways in BUFR.
# Therefore the code below might not work directly for other types of messages
# than the one used in the example. It is advised to use bufr_dump to
# understand the structure of the messages.
#
 
from __future__ import print_function
import traceback
import os, sys
import collections
 
from eccodes import *
 
#INPUT = '../../data/bufr/tropical_cyclone.bufr'
INPUT = sys.argv[1]
ODIR = sys.argv[2]
VERBOSE = 1  # verbose error reporting
 
data = collections.defaultdict(dict)
 
 
def example():
    # open BUFR file
    f = open(INPUT, 'rb')
 
    cnt = 0
 
    # loop for the messages in the file
    while 1:
        # get handle for message
        bufr = codes_bufr_new_from_file(f)
        if bufr is None:
            break
 
        print('**************** MESSAGE: ', cnt + 1, '  *****************')
 
        # we need to instruct ecCodes to expand all the descriptors
        # i.e. unpack the data values
        codes_set(bufr, 'unpack', 1)
 
        numObs = codes_get(bufr, "numberOfSubsets")
        year = codes_get(bufr, "year")
        month = codes_get(bufr, "month")
        day = codes_get(bufr, "day")
        hour = codes_get(bufr, "hour")
        minute = codes_get(bufr, "minute")
        YMDH = f'{year}{month:02d}{day:02d}{hour:02d}'
 
        print('Date and time: ', day, '.', month, '.', year, '  ', hour, ':', minute)

        longStormName = codes_get(bufr, "longStormName").strip()
        stormIdentifier = codes_get(bufr, "stormIdentifier")
        print(f'Storm : {longStormName} {stormIdentifier}')
        stormNumber = stormIdentifier[0:2]
        if stormIdentifier[2] == 'L':
            basin='AL'
        elif stormIdentifier[2] == 'E':
            basin='EP'
        elif stormIdentifier[2] == 'C':
            basin='CP'
        elif stormIdentifier[2] == 'W':
            basin='WP'
        elif stormIdentifier[2] == 'A':
            basin='IO'
        elif stormIdentifier[2] == 'B':
            basin='IO'
        elif stormIdentifier[2] == 'P':
            basin='SH'
        elif stormIdentifier[2] == 'S':
            basin='SH'
        else:
            basin='XX'

 
        # How many different timePeriod in the data structure?
        numberOfPeriods = 0
        while True:
            numberOfPeriods = numberOfPeriods + 1
            try:
                codes_get_array(bufr, "#%d#timePeriod" % numberOfPeriods)
            except CodesInternalError as err:
                break
                # the numberOfPeriods includes the analysis (period=0)
 
        # Get ensembleMemberNumber
        memberNumber = codes_get_array(bufr, "ensembleMemberNumber")
        memberNumberLen = len(memberNumber)
 
        # Observed Storm Centre (timePeriod=0)
        significance = codes_get(bufr, '#1#meteorologicalAttributeSignificance')
        latitudeCentre = codes_get(bufr, '#1#latitude')
        longitudeCentre = codes_get(bufr, '#1#longitude')
 
        if significance != 1:
            print('ERROR: unexpected #1#meteorologicalAttributeSignificance')
            return 1
 
        if (latitudeCentre == CODES_MISSING_DOUBLE) and (longitudeCentre == CODES_MISSING_DOUBLE):
            print('Observed storm centre position missing')
        else:
            print('Observed storm centre: latitude=', latitudeCentre, ' longitude=', longitudeCentre)
 
        # Location of storm in perturbed analysis
        significance = codes_get(bufr, '#2#meteorologicalAttributeSignificance')
 
        #if significance != 4:
        #    print('ERROR: unexpected #2#meteorologicalAttributeSignificance')
        #    return 1
 
        latitudeAnalysis = codes_get_array(bufr, '#2#latitude')
        longitudeAnalysis = codes_get_array(bufr, '#2#longitude')
        pressureAnalysis = codes_get_array(bufr, '#1#pressureReducedToMeanSeaLevel')
 
        # Location of Maximum Wind
        significance = codes_get(bufr, '#3#meteorologicalAttributeSignificance')
 
        if significance != 3:
            print('ERROR: unexpected #3#meteorologicalAttributeSignificance=', significance)
            return 1
 
        latitudeMaxWind0 = codes_get_array(bufr, '#3#latitude')
        longitudeMaxWind0 = codes_get_array(bufr, '#3#longitude')
        windMaxWind0 = codes_get_array(bufr, '#1#windSpeedAt10M')

        # Wind Radii: 1=18m/s, 2=26m/s, 3=33m/s
        windRadiiSpeedThresh1_0 = codes_get_array(bufr, '#1#windSpeedThreshold')
        windRadiiSpeedThresh2_0 = codes_get_array(bufr, '#2#windSpeedThreshold')
        windRadiiSpeedThresh3_0 = codes_get_array(bufr, '#3#windSpeedThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdNE1_0 = codes_get_array(bufr, '#1#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdSE1_0 = codes_get_array(bufr, '#2#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdSW1_0 = codes_get_array(bufr, '#3#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdNW1_0 = codes_get_array(bufr, '#4#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdNE2_0 = codes_get_array(bufr, '#5#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdSE2_0 = codes_get_array(bufr, '#6#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdSW2_0 = codes_get_array(bufr, '#7#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdNW2_0 = codes_get_array(bufr, '#8#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdNE3_0 = codes_get_array(bufr, '#9#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdSE3_0 = codes_get_array(bufr, '#10#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdSW3_0 = codes_get_array(bufr, '#11#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
        effectiveRadiusWithRespectToWindSpeedsAboveThresholdNW3_0 = codes_get_array(bufr, '#12#effectiveRadiusWithRespectToWindSpeedsAboveThreshold')
 
        if len(latitudeAnalysis) == len(memberNumber) and len(latitudeMaxWind0) == len(memberNumber):
            for k in range(len(memberNumber)):
                data[k][0] = [latitudeAnalysis[k], longitudeAnalysis[k], pressureAnalysis[k], latitudeMaxWind0[k],
                              longitudeMaxWind0[k], windMaxWind0[k], 
                              windRadiiSpeedThresh1_0[k], effectiveRadiusWithRespectToWindSpeedsAboveThresholdNE1_0[k],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdSE1_0[k], effectiveRadiusWithRespectToWindSpeedsAboveThresholdSW1_0[k],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdNW1_0[k], windRadiiSpeedThresh2_0[k],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdNE2_0[k], effectiveRadiusWithRespectToWindSpeedsAboveThresholdSE2_0[k],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdSW2_0[k], effectiveRadiusWithRespectToWindSpeedsAboveThresholdNW2_0[k],
                              windRadiiSpeedThresh3_0[k], effectiveRadiusWithRespectToWindSpeedsAboveThresholdNE3_0[k],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdSE3_0[k], effectiveRadiusWithRespectToWindSpeedsAboveThresholdSW3_0[k],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdNW3_0[k]]
 
        else:
            for k in range(len(memberNumber)):
                data[k][0] = [latitudeAnalysis[0], longitudeAnalysis[0], pressureAnalysis[0], latitudeMaxWind0[0],
                              longitudeMaxWind0[0], windMaxWind0[0],
                              windRadiiSpeedThresh1_0[0], effectiveRadiusWithRespectToWindSpeedsAboveThresholdNE1_0[0],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdSE1_0[0], effectiveRadiusWithRespectToWindSpeedsAboveThresholdSW1_0[0],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdNW1_0[0], windRadiiSpeedThresh2_0[0],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdNE2_0[0], effectiveRadiusWithRespectToWindSpeedsAboveThresholdSE2_0[0],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdSW2_0[0], effectiveRadiusWithRespectToWindSpeedsAboveThresholdNW2_0[0],
                              windRadiiSpeedThresh3_0[0], effectiveRadiusWithRespectToWindSpeedsAboveThresholdNE3_0[0],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdSE3_0[0], effectiveRadiusWithRespectToWindSpeedsAboveThresholdSW3_0[0],
                              effectiveRadiusWithRespectToWindSpeedsAboveThresholdNW3_0[0]]
 
        timePeriod = [0 for x in range(numberOfPeriods)]
        print(f'Number of Time Periods = {numberOfPeriods}')

        for i in range(1, numberOfPeriods):
            rank1 = i * 2 + 2
            rank3 = i * 2 + 3
            rankWST1 = i * 3 + 1
            rankWST2 = i * 3 + 2
            rankWST3 = i * 3 + 3
            rankRadNE1 = i * 12 + 1
            rankRadSE1 = i * 12 + 2
            rankRadSW1 = i * 12 + 3
            rankRadNW1 = i * 12 + 4
            rankRadNE2 = i * 12 + 5
            rankRadSE2 = i * 12 + 6
            rankRadSW2 = i * 12 + 7
            rankRadNW2 = i * 12 + 8
            rankRadNE3 = i * 12 + 9
            rankRadSE3 = i * 12 + 10
            rankRadSW3 = i * 12 + 11
            rankRadNW3 = i * 12 + 12

 
            ivalues = codes_get_array(bufr, "#%d#timePeriod" % i)
            print(ivalues)
 
            if len(ivalues) == 1:
                timePeriod[i] = ivalues[0]
            else:
                for j in range(len(ivalues)):
                    if ivalues[j] != CODES_MISSING_LONG:
                        timePeriod[i] = ivalues[j]
                        break
            print(timePeriod)
 
            # Location of the storm
            values = codes_get_array(bufr, "#%d#meteorologicalAttributeSignificance" % rank1)
            if len(values) == 1:
                significance = values[0]
            else:
                for j in range(len(values)):
                    if values[j] != CODES_MISSING_LONG:
                        significance = values[j]
                        break
 
            if significance == 1:
                lat = codes_get_array(bufr, "#%d#latitude" % rank1)
                lon = codes_get_array(bufr, "#%d#longitude" % rank1)
                press = codes_get_array(bufr, "#%d#pressureReducedToMeanSeaLevel" % (i + 1))
            else:
                print('ERROR: unexpected meteorologicalAttributeSignificance=', significance)
 
            # Location of maximum wind
            values = codes_get_array(bufr, "#%d#meteorologicalAttributeSignificance" % rank3)
            if len(values) == 1:
                significanceWind = values[0]
            else:
                for j in range(len(values)):
                    if values[j] != CODES_MISSING_LONG:
                        significanceWind = values[j]
                        break
 
            if significanceWind == 3:
                latWind = codes_get_array(bufr, "#%d#latitude" % rank3)
                lonWind = codes_get_array(bufr, "#%d#longitude" % rank3)
                wind10m = codes_get_array(bufr, "#%d#windSpeedAt10M" % (i + 1))
            else:
                print('ERROR: unexpected meteorologicalAttributeSignificance=', significanceWind)

            # Wind Radii for different intensity thresholds
            windRadiiSpeedThresh1 = codes_get_array(bufr, '#%d#windSpeedThreshold' % rankWST1)
            windRadiiSpeedThresh2 = codes_get_array(bufr, '#%d#windSpeedThreshold' % rankWST2)
            windRadiiSpeedThresh3 = codes_get_array(bufr, '#%d#windSpeedThreshold' % rankWST3)
            effectiveWindRadiusNE1 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadNE1)
            effectiveWindRadiusSE1 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadSE1)
            effectiveWindRadiusSW1 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadSW1)
            effectiveWindRadiusNW1 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadNW1)
            effectiveWindRadiusNE2 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadNE2)
            effectiveWindRadiusSE2 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadSE2)
            effectiveWindRadiusSW2 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadSW2)
            effectiveWindRadiusNW2 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadNW2)
            effectiveWindRadiusNE3 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadNE3)
            effectiveWindRadiusSE3 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadSE3)
            effectiveWindRadiusSW3 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadSW3)
            effectiveWindRadiusNW3 = codes_get_array(bufr, '#%d#effectiveRadiusWithRespectToWindSpeedsAboveThreshold' % rankRadNW3)
 
            # Arrange all required values for this time in a list
            for k in range(len(memberNumber)):
                data[k][i] = [lat[k], lon[k], press[k], latWind[k], lonWind[k], wind10m[k], 
                              windRadiiSpeedThresh1[k], effectiveWindRadiusNE1[k], effectiveWindRadiusSE1[k],
                              effectiveWindRadiusSW1[k], effectiveWindRadiusNW1[k], windRadiiSpeedThresh2[k],
                              effectiveWindRadiusNE2[k], effectiveWindRadiusSE2[k], effectiveWindRadiusSW2[k],
                              effectiveWindRadiusNW2[k], windRadiiSpeedThresh3[k], effectiveWindRadiusNE3[k],
                              effectiveWindRadiusSE3[k], effectiveWindRadiusSW3[k], effectiveWindRadiusNW3[k]]
 
 
            # ---------------------------------------- Print the values -------------
 
        for m in range(len(memberNumber)):
            #print("== Member  %d" % memberNumber[m])
            #print("step  latitude  longitude   pressure  latitude   longitude    wind")

            FNAME = f'{ODIR}/{longStormName.lower()}{stormIdentifier.lower()}.{YMDH}.trak.ecmo.atcfunix'
            write_flag = 0
            if not os.path.isfile(FNAME):
                write_flag = 1
                atcf = open(FNAME, 'a')
            else:
                print('The ATCF file already exists, so I won\'t overwrite it.')

            for s in range(len(timePeriod)):
                #print(data[m][s])
                if data[m][s][0] != CODES_MISSING_DOUBLE and data[m][s][1] != CODES_MISSING_DOUBLE:
                    TMP = data[m][s][0]
                    latitude=f'{int(TMP*10):>3d}N' if TMP>0 else f'{int(abs(TMP)*10):>3d}S'
                    TMP = data[m][s][1]
                    longitude=f'{int(TMP*10):>4d}E' if TMP>0 else f'{int(abs(TMP)*10):>4d}W'

                    print("{0}{1}{2}{3}{4}{5}{6:>3d}{7}{8}{9}{10}{11}{12:>3.0f}{13}{14:>4.0f}{15}{16:>4.0f}{17}{18:>4.0f}{19}{20:>4.0f}{21}{22:>4.0f}{23}".format(
                          basin, ', ', stormNumber, ', ', YMDH, ', 03, ECMO, ', timePeriod[s], ', ', latitude, ', ', longitude, ', ', 1.94384*data[m][s][5], ', ',
                          0.01*data[m][s][2], ', XX,  34, NEQ, ', data[m][s][7]/1852, ', ', data[m][s][8]/1852, ', ', data[m][s][9]/1852, ', ', data[m][s][10]/1852, ', '))

                    if write_flag == 1:
                        atcf.write("{0}{1}{2}{3}{4}{5}{6:>3d}{7}{8}{9}{10}{11}{12:>3.0f}{13}{14:>4.0f}{15}{16:>4.0f}{17}{18:>4.0f}{19}{20:>4.0f}{21}{22:>4.0f}{23}\n".format(
                                   basin, ', ', stormNumber, ', ', YMDH, ', 03, ECMO, ', timePeriod[s], ', ', latitude, ', ', longitude, ', ', 1.94384*data[m][s][5], ', ',
                                   0.01*data[m][s][2], ', XX,  34, NEQ, ', data[m][s][7]/1852, ', ', data[m][s][8]/1852, ', ', data[m][s][9]/1852, ', ', data[m][s][10]/1852, ', '))

                    # Write the 50-kt wind radii line, if necessary.
                    if ( data[m][s][12] != CODES_MISSING_DOUBLE and data[m][s][13] != CODES_MISSING_DOUBLE
                         and data[m][s][14] != CODES_MISSING_DOUBLE and data[m][s][15] != CODES_MISSING_DOUBLE):

                        if data[m][s][12] == 0 and data[m][s][13] == 0 and data[m][s][14] == 0 and data[m][s][15] == 0:  continue

                        print("{0}{1}{2}{3}{4}{5}{6:>3d}{7}{8}{9}{10}{11}{12:>3.0f}{13}{14:>4.0f}{15}{16:>4.0f}{17}{18:>4.0f}{19}{20:>4.0f}{21}{22:>4.0f}{23}".format(
                              basin, ', ', stormNumber, ', ', YMDH, ', 03, ECMO, ', timePeriod[s], ', ', latitude, ', ', longitude, ', ', 1.94384*data[m][s][5], ', ',
                              0.01*data[m][s][2], ', XX,  50, NEQ, ', data[m][s][12]/1852, ', ', data[m][s][13]/1852, ', ', data[m][s][14]/1852, ', ', data[m][s][15]/1852, ', '))

                        if write_flag == 1:
                            atcf.write("{0}{1}{2}{3}{4}{5}{6:>3d}{7}{8}{9}{10}{11}{12:>3.0f}{13}{14:>4.0f}{15}{16:>4.0f}{17}{18:>4.0f}{19}{20:>4.0f}{21}{22:>4.0f}{23}\n".format(
                                       basin, ', ', stormNumber, ', ', YMDH, ', 03, ECMO, ', timePeriod[s], ', ', latitude, ', ', longitude, ', ', 1.94384*data[m][s][5], ', ',
                                       0.01*data[m][s][2], ', XX,  50, NEQ, ', data[m][s][12]/1852, ', ', data[m][s][13]/1852, ', ', data[m][s][14]/1852, ', ', data[m][s][15]/1852, ', '))

                        # Write the 64-kt wind radii line, if necessary
                        if ( data[m][s][17] != CODES_MISSING_DOUBLE and data[m][s][18] != CODES_MISSING_DOUBLE
                             and data[m][s][19] != CODES_MISSING_DOUBLE and data[m][s][20] != CODES_MISSING_DOUBLE):

                            if data[m][s][17] == 0 and data[m][s][18] == 0 and data[m][s][19] == 0 and data[m][s][20] == 0:  continue
    
                            print("{0}{1}{2}{3}{4}{5}{6:>3d}{7}{8}{9}{10}{11}{12:>3.0f}{13}{14:>4.0f}{15}{16:>4.0f}{17}{18:>4.0f}{19}{20:>4.0f}{21}{22:>4.0f}{23}".format(
                                  basin, ', ', stormNumber, ', ', YMDH, ', 03, ECMO, ', timePeriod[s], ', ', latitude, ', ', longitude, ', ', 1.94384*data[m][s][5], ', ',
                                  0.01*data[m][s][2], ', XX,  64, NEQ, ', data[m][s][17]/1852, ', ', data[m][s][18]/1852, ', ', data[m][s][19]/1852, ', ', data[m][s][20]/1852, ', '))

                            if write_flag == 1:
                                atcf.write("{0}{1}{2}{3}{4}{5}{6:>3d}{7}{8}{9}{10}{11}{12:>3.0f}{13}{14:>4.0f}{15}{16:>4.0f}{17}{18:>4.0f}{19}{20:>4.0f}{21}{22:>4.0f}{23}\n".format(
                                           basin, ', ', stormNumber, ', ', YMDH, ', 03, ECMO, ', timePeriod[s], ', ', latitude, ', ', longitude, ', ', 1.94384*data[m][s][5], ', ',
                                           0.01*data[m][s][2], ', XX,  64, NEQ, ', data[m][s][17]/1852, ', ', data[m][s][18]/1852, ', ', data[m][s][19]/1852, ', ', data[m][s][20]/1852, ', '))

            if write_flag == 1:  atcf.close()
 
                # -----------------------------------------------------------------------
        cnt += 1
 
        # release the BUFR message
        codes_release(bufr)
 
    # close the file
    f.close()
 
 
def main():
    try:
        example()
    except CodesInternalError as err:
        if VERBOSE:
            traceback.print_exc(file=sys.stderr)
        else:
            sys.stderr.write(err.msg + '\n')
 
        return 1
 
 
if __name__ == "__main__":
    sys.exit(main())
