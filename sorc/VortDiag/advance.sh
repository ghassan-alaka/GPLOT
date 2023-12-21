#!/bin/sh -f

# A script to advance given time forward or backward in minute increments
# 6 inputs: YYYY MM DD HH MM(start year,month,day,hour,min) H2(+/- minutes to advance)
# Output is in YYYYMMDDHHMM format.

yy=$1
mo=$2
dd=$3
hh=$4
mn=$5
md=$6


addhr=0
newhh=$hh
newdd=$dd
newmo=$mo

newmn=`expr $mn + $md`


if [ $newmn -ge 60 ]
then
   addhr=`expr $newmn / 60`
   newmn=`expr $newmn % 60`
fi


newhh=`expr $hh + $addhr`

 
if [ $newhh -ge 24 ]
then
   addday=`expr $newhh / 24`
   newhh=`expr $newhh % 24`
   newdd=`expr $newdd + $addday`
   if [ $newdd -lt 10 ]
             then
                newdd=0${newdd}
             fi

   if [ $newdd -gt 30 ]
   then
       if [ $newmo -eq "04" ]
       then
           newmo=05
           newdd=`expr $newdd - 30`
           if [ $newdd -lt 10 ]
             then
                newdd=0${newdd}
             fi

       elif [ $newmo -eq "06" ]
       then
            newmo=07
            newdd=`expr $newdd - 30`
            if [ $newdd -lt 10 ]
             then
                newdd=0${newdd}
             fi
     
       elif [ $newmo -eq "09" ]
       then
          newmo=10
          newdd=`expr $newdd - 30`
          if [ $newdd -lt 10 ]
             then
                newdd=0${newdd}
             fi

       elif [ $newmo -eq "11" ]
       then
          newmo=12
          newdd=`expr $newdd - 30`
             if [ $newdd -lt 10 ]
             then
                newdd=0${newdd}
             fi

       fi
    fi

    if [ $newdd -gt 31 ]
    then
       if [ $newmo -eq "05" ]
       then
          newmo=06
          newdd=`expr $newdd - 31`
             if [ $newdd -lt 10 ]
             then
                newdd=0${newdd}
             fi
       elif [ $newmo -eq "07" ]
       then
           newmo=08
           newdd=`expr $newdd - 31`
        elif [ $newmo -eq "08" ]
        then
           newmo=09
           newdd=`expr $newdd - 31`
           if [ $newdd -lt 10 ]
             then
                newdd=0${newdd}
             fi
        elif [ $newmo -eq "10" ]
        then
           newmo=11
           newdd=`expr $newdd - 31`
        fi
     fi
fi

if [ $newhh -lt 10 ]
then
   newhh=0${newhh}
fi
if [ $newmn -lt 10 ]
then
  newmn=0${newmn}
fi
if [ $newhh -eq 0 ]
then
  newhh=00
fi
if [ $newmn -eq 0 ]
then
  newmn=00
fi

  echo ${yy}${newmo}${newdd}${newhh}${newmn}

exit
