#!/bin/csh
set efiles = `ls *.sh`
foreach efile ( $efiles )
	echo $efile
	sed "s#sjet,vjet,xjet,kjet#u1-compute#g; s#hur-aoml#aoml-hafs1#g; s#/lfs1/projects/aoml-hafs1/Ghassan.Alaka/GPLOT/#/scratch3/AOML/aoml-hafs1/role.aoml-hafs1/software/GPLOT/#g" $efile > temp
	\mv temp $efile
	\chmod +x $efile
end
