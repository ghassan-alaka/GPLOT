

in = asciiread("temp.dat",-1,"string")
in:= toint(str_get_cols(in,4,6))
;in:= in(:,0:2)
print(in)
out=new((/15,3/),integer)

do i=0,14
out(i,0)=in(i+30)
out(i,1)=in(i+15)
out(i,2)=in(i)
end do

write_matrix(out,"3I4",False)
