function set230Colors(args)
**230 colors 37H brightness temperature colorbar from NRL website
**Takes 1 argument: 1 or '' means set RGB colors, ccols and clevs (default), 2 means only set RGB colors, 3 means only set ccols, and 4
**means only set clevs.

option=subwrd(args,1)
if(option='' | (option!=1 & option!=2 & option!=3 & option!=4));option=1;endif

if(option=1 | option=2)

** Create RGB colors sequence

"set rgb 209 255 0 0"
"set rgb 210 242 0 0"
"set rgb 211 230 0 0"
"set rgb 212 217 0 0"
"set rgb 213 204 0 0"
"set rgb 214 191 0 0"
"set rgb 215 178 0 0"
"set rgb 216 166 0 0"
"set rgb 217 153 0 0"
"set rgb 218 140 0 0"
"set rgb 219 128 0 0"
"set rgb 220 115 0 0"
"set rgb 221 102 0 0"
"set rgb 222 89 0 0"
"set rgb 223 76 0 0"
"set rgb 224 64 0 0"
"set rgb 225 51 0 0"
"set rgb 226 38 0 0"
"set rgb 227 26 0 0"
"set rgb 228 13 0 0"
"set rgb 229 1 0 0"
endif


if(option=1 | option=3)
'set ccols 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 1'
endif
** Create some countour levels
if(option=1 | option=4)
"set clevs 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280"

endif

return
