function set230Colors(args)
**230 colors 37H brightness temperature colorbar from NRL website
**Takes 1 argument: 1 or '' means set RGB colors, ccols and clevs (default), 2 means only set RGB colors, 3 means only set ccols, and 4
**means only set clevs.

option=subwrd(args,1)
if(option='' | (option!=1 & option!=2 & option!=3 & option!=4));option=1;endif

if(option=1 | option=2)

** Create RGB colors sequence

"set rgb 209 0  0 0"
"set rgb 210 0  13 0"
"set rgb 211 0  26 0"
"set rgb 212 0  38 0"
"set rgb 213 0  51 0"
"set rgb 214 0  64 0"
"set rgb 215 0  76 0"
"set rgb 216 0  89 0"
"set rgb 217 0  102 0"
"set rgb 218 0  115 0"
"set rgb 219 0  128 0"
"set rgb 220 0  140 0"
"set rgb 221 0  153 0"
"set rgb 222 0  166 0"
"set rgb 223 0  178 0"
"set rgb 224 0  191 0"
"set rgb 225 0  204 0"
"set rgb 226 0  217 0"
"set rgb 227 0  230 0"
"set rgb 228 0  242 0"
"set rgb 229 0  255 0"

endif


if(option=1 | option=3)
"set ccols 1 209 210 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229" 
endif
** Create some countour levels
if(option=1 | option=4)
"set clevs 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290"

endif

return
