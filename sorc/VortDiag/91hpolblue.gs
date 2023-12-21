function set230Colors(args)
**230 colors 37H brightness temperature colorbar from NRL website
**Takes 1 argument: 1 or '' means set RGB colors, ccols and clevs (default), 2 means only set RGB colors, 3 means only set ccols, and 4
**means only set clevs.

option=subwrd(args,1)
if(option='' | (option!=1 & option!=2 & option!=3 & option!=4));option=1;endif

if(option=1 | option=2)

** Create RGB colors sequence

"set rgb 169 0 0 255 "
"set rgb 170 0 0 251 "
"set rgb 171 0 0 247 "
"set rgb 172 0 0 242 "
"set rgb 173 0 0 238 "
"set rgb 174 0 0 234 "
"set rgb 175 0 0 230 "
"set rgb 176 0 0 225 "
"set rgb 177 0 0 221 "
"set rgb 178 0 0 217 "
"set rgb 179 0 0 213 "
"set rgb 180 0 0 208 "
"set rgb 181 0 0 204 "
"set rgb 182 0 0 200 "
"set rgb 183 0 0 196 "
"set rgb 184 0 0 191 "
"set rgb 185 0 0 187 "
"set rgb 186 0 0 182 "
"set rgb 187 0 0 178 "
"set rgb 188 0 0 174 "
"set rgb 189 0 0 170 "
"set rgb 190 0 0 166 "
"set rgb 191 0 0 161 "
"set rgb 192 0 0 157 "
"set rgb 193 0 0 153 "
"set rgb 194 0 0 149 "
"set rgb 195 0 0 144 "
"set rgb 196 0 0 140 "
"set rgb 197 0 0 136 "
"set rgb 198 0 0 132 "
"set rgb 199 0 0 128 "
"set rgb 200 0 0 123 "
"set rgb 201 0 0 119 "
"set rgb 202 0 0 115 "
"set rgb 203 0 0 110 "
"set rgb 204 0 0 106 "
"set rgb 205 0 0 102 "
"set rgb 206 0 0 98 "
"set rgb 207 0 0 93 "
"set rgb 208 0 0 89 "
"set rgb 209 0 0 85 "
"set rgb 210 0 0 81 "
"set rgb 211 0 0 76 "
"set rgb 212 0 0 72 "
"set rgb 213 0 0 68 "
"set rgb 214 0 0 64 "
"set rgb 215 0 0 60 "
"set rgb 216 0 0 55 "
"set rgb 217 0 0 51 "
"set rgb 218 0 0 47 "
"set rgb 219 0 0 42 "
"set rgb 220 0 0 38 "
"set rgb 221 0 0 34 "
"set rgb 222 0 0 30 "
"set rgb 223 0 0 26 "
"set rgb 224 0 0 21 "
"set rgb 225 0 0 17 "
"set rgb 226 0 0 13 "
"set rgb 227 0 0 8 "
"set rgb 228 0 0 4 "
"set rgb 229 0 0 0 "

endif


if(option=1 | option=3)
"set ccols 1 229 228 227 226 225 224 223 222 221 220 219 218 217 216 215 214 213 212 211 210 209 208 207 206 205 204 203 202 201 200 199 198 197 196 195 194 193 192 191 190 189 188 187 186 185 184 183 182 181 180 179 178 177 176 175 174 173 172 171 170 169" 
endif
** Create some countour levels
if(option=1 | option=4)
"set clevs 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300"

endif

return
