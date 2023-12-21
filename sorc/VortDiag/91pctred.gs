function set230Colors(args)
**230 colors 37H brightness temperature colorbar from NRL website
**Takes 1 argument: 1 or '' means set RGB colors, ccols and clevs (default), 2 means only set RGB colors, 3 means only set ccols, and 4
**means only set clevs.

option=subwrd(args,1)
if(option='' | (option!=1 & option!=2 & option!=3 & option!=4));option=1;endif

if(option=1 | option=2)

** Create RGB colors sequence

"set rgb 139 0 0 0"
"set rgb 140 2 0 0"
"set rgb 141 5 0 0"
"set rgb 142 8 0 0"
"set rgb 143 11 0 0"
"set rgb 144 14 0 0"
"set rgb 145 17 0 0"
"set rgb 146 20 0 0"
"set rgb 147 23 0 0"
"set rgb 148 26 0 0"
"set rgb 149 28 0 0"
"set rgb 150 31 0 0"
"set rgb 151 34 0 0"
"set rgb 152 37 0 0"
"set rgb 153 40 0 0"
"set rgb 154 43 0 0"
"set rgb 155 45 0 0"
"set rgb 156 48 0 0"
"set rgb 157 51 0 0"
"set rgb 158 54 0 0"
"set rgb 159 57 0 0"
"set rgb 160 60 0 0"
"set rgb 161 62 0 0"
"set rgb 162 65 0 0"
"set rgb 163 68 0 0"
"set rgb 164 71 0 0"
"set rgb 165 74 0 0"
"set rgb 166 77 0 0"
"set rgb 167 79 0 0"
"set rgb 168 82 0 0"
"set rgb 169 85 0 0"
"set rgb 170 88 0 0"
"set rgb 171 91 0 0"
"set rgb 172 94 0 0"
"set rgb 173 96 0 0"
"set rgb 174 99 0 0"
"set rgb 175 102 0 0"
"set rgb 176 105 0 0"
"set rgb 177 108 0 0"
"set rgb 178 111 0 0"
"set rgb 179 113 0 0"
"set rgb 180 116 0 0"
"set rgb 181 119 0 0"
"set rgb 182 122 0 0"
"set rgb 183 125 0 0"
"set rgb 184 128 0 0"
"set rgb 185 130 0 0"
"set rgb 186 133 0 0"
"set rgb 187 136 0 0"
"set rgb 188 139 0 0"
"set rgb 189 142 0 0"
"set rgb 190 145 0 0"
"set rgb 191 147 0 0"
"set rgb 192 150 0 0"
"set rgb 193 153 0 0"
"set rgb 194 156 0 0"
"set rgb 195 159 0 0"
"set rgb 196 162 0 0"
"set rgb 197 164 0 0"
"set rgb 198 167 0 0"
"set rgb 199 170 0 0"
"set rgb 200 173 0 0"
"set rgb 201 176 0 0"
"set rgb 202 179 0 0"
"set rgb 203 181 0 0"
"set rgb 204 184 0 0"
"set rgb 205 187 0 0"
"set rgb 206 190 0 0"
"set rgb 207 193 0 0"
"set rgb 208 196 0 0"
"set rgb 209 198 0 0"
"set rgb 210 201 0 0"
"set rgb 211 204 0 0"
"set rgb 212 207 0 0"
"set rgb 213 210 0 0"
"set rgb 214 213 0 0"
"set rgb 215 215 0 0"
"set rgb 216 218 0 0"
"set rgb 217 221 0 0"
"set rgb 218 224 0 0"
"set rgb 219 227 0 0"
"set rgb 220 230 0 0"
"set rgb 221 232 0 0"
"set rgb 222 235 0 0"
"set rgb 223 238 0 0"
"set rgb 224 241 0 0"
"set rgb 225 244 0 0"
"set rgb 226 247 0 0"
"set rgb 227 249 0 0"
"set rgb 228 252 0 0"
"set rgb 229 255 0 0"
endif


if(option=1 | option=3)
"set ccols 229 228 227 226 225 224 223 222 221 220 219 218 217 216 215 214 213 212 211 210 209 208 207 206 205 204 203 202 201 200 199 198 197 196 195 194 193 192 191 190 189 188 187 186 185 184 183 182 181 180 179 178 177 176 175 174 173 172 171 170 169 168 167 166 165 164 163 162 161 160 159 158 157 156 155 154 153 152 151 150 149 148 147 146 145 144 143 142 141 140 139 1" 
endif
** Create some countour levels
if(option=1 | option=4)
"set clevs 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310"

endif

return
