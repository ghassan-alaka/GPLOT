;
; File Name: ECMWF_combine.ncl
;
; Description:  This script combines individul GRIB2 input files
;		from ECMWF into a single NetCDF input file for
;		each respective forecast cycle and lead time.
;
; Created By:    Ghassan Alaka Jr.
; Date Created:  June 26, 2019
;
;================================================================

;================================================================
; Load scripts
;================================================================

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

;================================================================
; Begin the code.
;================================================================

begin

print("MSG: ECMWF_combine.ncl started at "+systemfunc("date"))
print("")


; Read CYCLE as a command line argument
if(.not.isvar("CYCLE"))then	print("ERROR: CYCLE is undefined. Please define it on the command line.")
				exit
end if

; Get important directories from the environment
ODIR = systemfunc("echo $PWD")
TMPDIR = ODIR+"/TMP."+systemfunc("date +%N")
system("mkdir -p "+TMPDIR)

; Setup alternate CYCLE variable
YMDHms = CYCLE+"0000"

; Setup the forecast hours for the ECMWF data. This is hard-coded.
FHR = ispan(0,240,24)
FHR_STR = (/"an","24h","48h","72h","96h","120h","144h","168h","192h","216h","240h"/)
NFHR = dimsizes(FHR)

; Setup file names for ECMWF data
VAR_STR = (/"msl","t_850hPa","u_850hPa","v_850hPa","gh_500hPa"/)
VAR = (/"PRES_P0_L101_GLL0","TMP_P0_L100_GLL0","UGRD_P0_L100_GLL0","VGRD_P0_L100_GLL0","HGT_P0_L100_GLL0"/)
NVAR = dimsizes(VAR_STR)


; Loop over all forecast lead times
do fff = 0,NFHR-1
	print("MSG: Working on this lead time --> "+FHR_STR(fff))

	; Look for raw ECMWF data
	do vvv = 0,NVAR-1
		; Define the input file and check if it exists.
		IFILE = systemfunc("ls "+ODIR+"/"+CYCLE+"/*C_ECMF_"+YMDHms+"_"+FHR_STR(fff)+"_"+VAR_STR(vvv)+"_global_0p5deg.grib2 2> /dev/null")
		if(ismissing(IFILE))then   continue   end if

		; Read the input file
		f = addfile(IFILE,"r")
		;print(f)


		; If the output file hasn't already been created, do that here.
		if(.not.isvar("OFILE"))then
			OFILE = TMPDIR+"/ecmwf."+CYCLE+".global.0p5.f"+sprinti("%0.3i",FHR(fff))+".nc"
			OFILE2 = ODIR+"/"+CYCLE+"/ecmwf."+CYCLE+".global.0p5.f"+sprinti("%0.3i",FHR(fff))+".nc"

			;setfileoption("nc","format","netcdf4")
			;setfileoption("nc","headerReserveSpace",64000)
			;setfileoption("nc","preFill",False)

			if(isfilepresent(OFILE))then   system("rm -f "+OFILE)   end if

			fout = addfile(OFILE,"c")

			setfileoption(fout,"DefineMode",True)
		end if

		; Get the latitude & longitude once.
		; Write them to the outfile on the fly.
		if(.not.isvar("lat_0") .and. .not.isvar("lon_0"))then
			setfileoption(fout,"DefineMode",True)

			lat_0 = f->lat_0
			nlat = dimsizes(lat_0)

			lon_0 = f->lon_0
			nlon = dimsizes(lon_0)

			dim_names = (/"lat_0","lon_0"/)
			dim_sizes = (/nlat,nlon/)
			dim_unlimited = (/False,False/)
			filedimdef(fout,dim_names,dim_sizes,dim_unlimited)

			;mlat = nlat/10
			;mlon = nlon/10
			;chunk_sizes = (/mlat,mlon/)
			;filechunkdimdef(fout,dim_names,chunk_sizes,dim_unlimited)

			filevardef(fout,"lat_0","float","lat_0")
			filevardef(fout,"lon_0","float","lon_0")

			latatts = True
			copy_VarAtts(lat_0,latatts)
			filevarattdef(fout,"lat_0",latatts)

			lonatts = True
			copy_VarAtts(lon_0,lonatts)
			filevarattdef(fout,"lon_0",lonatts)

			fout->lat_0 = (/lat_0/)
			fout->lon_0 = (/lon_0/)

			;printVarSummary(LAT)
			;exit
		end if

		; Read the main variable
		THIS_VAR := f->$VAR(vvv)$

		; Define the main variable in the outfile
		filevardef(fout,VAR(vvv),"float",(/"lat_0","lon_0"/))

		; Define main variable attributes in the outfile.
		varatts = 0.0
		copy_VarAtts(THIS_VAR,varatts)
		filevarattdef(fout,VAR(vvv),varatts)

		; Specify how to write the main variable.
		;filevarchunkdef(fout, VAR(vvv), chunk_sizes)
		;filevarcompressleveldef(fout, VAR(vvv), 2)

		; Write the main variable to the outfile.
		fout->$VAR(vvv)$ = (/THIS_VAR/)

		; Delete the outfile to close it.
		setfileoption(fout,"DefineMode",False)
		;delete(fout)


		; Remove original linked file
		system("rm -f "+IFILE)

	end do


	; Check if the file is already present and if it's different
	; than the one we just created.
	if(isfilepresent(OFILE2))then
		test = systemfunc("diff "+OFILE+" "+OFILE2)
		if(.not.ismissing(test(0)))then
			system("mv "+OFILE+" "+OFILE2)
		else
			system("rm -f "+OFILE)
		end if
	else
		system("mv "+OFILE+" "+OFILE2)
	end if


	; Cleanup
	if(isvar("OFILE"))then   delete(OFILE)   end if
	if(isvar("lat_0"))then   delete(lat_0)   end if
	if(isvar("lon_0"))then   delete(lon_0)   end if

end do


; Remove temporary directory
system("rm -rf "+TMPDIR)




;================================================================
; Complete the code.
;================================================================

print("")
print("MSG: ECMWF_combine.ncl completed at "+systemfunc("date"))

exit

end
