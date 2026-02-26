# GPLOT NCL → Python Conversion Journal

**Project:** Convert all NCL scripts in `sorc/GPLOT/ncl/` to Python equivalents in `sorc/GPLOT/python/`
**Started:** 2026-02-25
**Last Updated:** 2026-02-26 (session 7)

---

## Quick Status

| NCL File | Lines | Python Target | Status | Notes |
|---|---|---|---|---|
| `ECMWF_combine.ncl` | 192 | `python/ECMWF_combine.py` | ✅ Done | |
| `colormaps/calculate.ncl` | — | `python/colormaps/calculate.py` | ✅ Done | |
| `GPLOT_util_legacy.ncl` | 314 | `python/modules/gplot_util_legacy.py` | ✅ Done | |
| `GPLOT_main.ncl` | 842 | `python/modules/gplot_main.py` | ✅ Done | 479 lines; 2 public functions; syntax verified |
| `GPLOT_func.ncl` | 2227 | `python/modules/gplot_func.py` | ✅ Done | 1272 lines; 21 public functions; syntax verified |
| `GPLOT_util.ncl` | 3673 | `python/modules/gplot_util.py` | ✅ Done | 2306 lines; 40 public functions; syntax verified |
| `GPLOT_ships.ncl` | 3072 | `python/GPLOT_ships.py` | ✅ Done | 1616 lines; 27 functions; syntax verified |
| `GPLOT_stats.ncl` | 5383 | `python/GPLOT_stats.py` | ✅ Done | 1624 lines; 32 functions; syntax verified |
| `GPLOT_maps.ncl` | 3269 | `python/GPLOT_maps.py` | ✅ Done | 1512 lines; top-level driver; syntax verified |
| Shell/batch scripts | — | — | ⏳ Pending | Update callers to invoke Python instead of NCL |

---

## Dependency Order (convert in this order)

NCL scripts load each other; the Python modules must be converted bottom-up:

```
1. GPLOT_util.ncl          ← no NCL deps (do first)
2. GPLOT_func.ncl          ← loads GPLOT_util.ncl
3. GPLOT_main.ncl          ← loads GPLOT_func.ncl
4. GPLOT_util_legacy.ncl   ← standalone legacy shim (low priority)
5. GPLOT_maps.ncl          ← loads GPLOT_util + GPLOT_func + GPLOT_main  (top-level script)
6. GPLOT_ships.ncl         ← loads GPLOT_util + GPLOT_func + GPLOT_main  (top-level script)
7. GPLOT_stats.ncl         ← loads GPLOT_util + GPLOT_func + GPLOT_main  (top-level script)
```

---

## Python Style Conventions (from completed files)

- Shebang: `#!/usr/bin/env python`
- Module-level docstring with description, usage, and `Original NCL:` reference
- Imports: stdlib first, then third-party (numpy, xarray, matplotlib, etc.)
- Top-level scripts wrapped in `def main()` with `if __name__ == "__main__": main()`
- Library modules expose functions directly (no class wrappers unless warranted)
- NCL `print("MSG: ...")` → `print(f"MSG: ...")`
- NCL `systemfunc("date")` → `datetime.datetime.now()`
- NCL `getenv("VAR")` → `os.environ.get("VAR")`
- NCL resource blocks → matplotlib/Cartopy keyword dicts
- NCL `fval = 1e+20` missing-value convention → `np.nan` or `np.ma.masked`
- NCL external Fortran (`.so`) calls → scipy/numpy equivalents where possible

---

## File-by-File Notes

### GPLOT_util_legacy.ncl (314 lines) — 🔄 Resume here next session

**Purpose:** Deprecated legacy functions; kept for reference only. Header says "Do not load unless you have good reason."

**Functions:**
- `findVarName` (line 13) — maps variable/level strings to numeric codes via a master lookup table

**Python target:** `python/modules/gplot_util_legacy.py`
**Action:** Thin module with a deprecation warning on import; port `findVarName` as a dict lookup.

---

### GPLOT_main.ncl (842 lines) — 🔄 Resume here next session

**Purpose:** Higher-level functions; loads GPLOT_func.ncl.

**Functions:**
- `findCenter` (line 38) — finds TC center from model fields using CENTROID external Fortran lib
- `Read_Master_Namelist` (line 214) — reads the GPLOT master namelist file

**Python target:** `python/modules/gplot_main.py`
**Dependencies:** `gplot_func.py` (must exist first)
**Action:** `findCenter` will need scipy/custom centroid logic (replaces Fortran CENTROID.so). `Read_Master_Namelist` → parse namelist text file with Python.

---

### GPLOT_func.ncl (2227 lines) — ⏳ Pending

**Purpose:** Primary I/O and plotting helper library.

**Functions (21 total):**
- `add_disclaimer` — add text annotation to plot
- `add_graphic_title` — add variable title
- `add_model_title` — add model/ensemble label
- `add_mslp_label` — add MSLP annotation
- `add_sid_label` — add storm-ID annotation
- `add_storm_marker` — add intensity symbol (hurricane/TS/low)
- `add_storm_title` — add multi-storm title block
- `add_time_title` — add time string annotation
- `add_vmax_label` — add VMAX annotation
- `adeckRead` — read ATCF A-deck; returns (lat, lon, lead, vmax, pmin) array
- `atcfRead` — read generic ATCF file filtered by cycle/model
- `bdeckRead` — read ATCF B-deck best-track
- `get_dim_lat` — build latitude axis for 9 domain types
- `get_dim_lon` — build longitude axis with pivot/flip logic
- `get_uniq_ind` — deduplicate index list
- `getInputFile` — find first NetCDF file containing requested variable
- `getTopo` — load & regrid NCARG topography
- `getVar2d` — primary 2D variable reader (UV, RVO, PV, scalars; vortex removal; unit conversion)
- `getVar3d` — 3D variable reader (lev/lat/lon slice; UV magnitude)
- `getVarXC` — vertical cross-section reader (zonal or meridional)
- `nml_read` — parse GPLOT namelist string array

**Python target:** `python/modules/gplot_func.py`
**Dependencies:** `gplot_util.py`
**Key NCL→Python mappings:**
- ATCF reading → plain file I/O or `pandas.read_csv`
- `addpoly`, `gsn_add_text` etc. → Matplotlib `ax.text`, `ax.annotate`
- Fortran `mwavg.so` / `hbfilter.so` → scipy.ndimage or custom numpy
- `regrid2` → `xarray` + `scipy.interpolate` or `pyresample`

---

### GPLOT_util.ncl (3673 lines) — ✅ Done

**Purpose:** Core utility library; lowest-level, no NCL deps.

**All 40 functions (fully catalogued):**
- `load_constants` — physical constants dict
- `allMasters` — basin master lists
- `arrow` — draw arrow with arrowhead on a plot
- `basinCodes` — map basin name↔code (long_name / XX / X)
- `calcTheta` — potential temperature
- `changeTimeFmt` — reformat time strings between 9 supported formats
- `chkCmdInputs` — validate command-line args, set defaults
- `circle_ll` — draw lat/lon circle on map
- `defineCMAP_fill` — load & optionally slice colormap array
- `defineCMAP_name` — colormap name selection by variable/level
- `defineLevels` — contour levels by variable/level
- `filter121` — 1-2-1 smoother (numpy equivalent of Fortran filter121.so)
- `findVarName` — reads `tbl/Vtable.*` files to resolve model-specific var names
- `GenPlotRes` — generate matplotlib kwargs dict by plot type (0-9) and overlay flag
- `getAutoDir` — hard-coded JET/NOAA paths; largely legacy
- `getDmnBds` — domain lat/lon bounding boxes (27 named domains)
- `getDmnInfo` — domain metadata: bdstype, maxH, maxL, llbox
- `getExptInfo` — reads `tbl/ExptInfo.dat` for experiment title info
- `getIDIR` — walk directory tree to build input path
- `getFileTag` / `getFileTag1` / `getFileTag2` / `getFileTag3` — build file glob patterns
- `getInvestSID` — reads `tbl/SIDs_Old_New.dat` to look up TC invest SID
- `getLatLonLbl` / `getLatLonLbl2` — build axis tick label arrays for lat/lon
- `getModelInfo` — reads `tbl/ModelInfo.dat` for model color/marker/title
- `getModelInfo2` — hardcoded model info table (color, marker, title, interp_code)
- `getPlotTitle` — hardcoded plot title strings by short_name key (~40 entries)
- `getStmThin` — reads `tbl/StreamlineThin.dat` for streamline thinning
- `hbfilter` — Kurihara vortex removal filter (numpy port of hbfilter Fortran)
- `isStrSubset2` — vectorized string-contains check
- `level_convert` — convert levels between Pa/hPa and str/float/int types
- `printMaxMin` — print max and min of array
- `remove_duplicates` — sort and deduplicate numeric array
- `StatPlotRes` — stat/track plot matplotlib kwargs (18 type codes)
- `stringOut` — format array as comma-separated string
- `testPlot_2d_map` / `testPlot_map` / `testPlot_contour` — debug plot helpers

**Python target:** `python/modules/gplot_util.py`
**Key NCL→Python mappings:**
- NCL resource objects → Python dicts
- `gsn_add_polyline` → `ax.plot`
- `gsn_csm_contour_map` → `ax.contourf` + Cartopy
- `wgt_runave_n_Wrap` → `scipy.ndimage.uniform_filter1d` or manual numpy
- `asciiread` / `str_get_field` → `open().readlines()` + `str.split()`
- `systemfunc` → `os.environ.get` or `subprocess`
- `fspan` → `numpy.linspace`
**Status:** ✅ Complete. `python/modules/gplot_util.py` written (2306 lines). Syntax verified.

---

### GPLOT_maps.ncl (3269 lines) — ⏳ Pending

**Purpose:** Top-level script; produces 2D map graphics from model output.
**Loads:** GPLOT_util + GPLOT_func + GPLOT_main
**External:** `sph2cart.so` Fortran lib (spherical→Cartesian)
**Python target:** `python/GPLOT_maps.py`
**Note:** No named NCL functions; monolithic `begin...end` block. Convert to `main()`.

---

### GPLOT_ships.ncl (3072 lines) — ✅ Done

**Purpose:** Top-level script; extracts SHIPS/LSDIAG diagnostic variables and writes `.DAT` files + graphics.
**Loads:** GPLOT_util + GPLOT_func + GPLOT_main
**External:** `sph2cart.so` → replaced with `sph2cart()` using `scipy.interpolate.RegularGridInterpolator`
**Python target:** `python/GPLOT_ships.py` (1616 lines, 27 functions)

**Key design decisions:**
- `SPH2CART` Fortran external → `sph2cart()` using haversine-based flat-Earth projection + bilinear interpolation
- Annular averages: `annular_avg()` / `annular_avg_uv()` operating on Cartesian grids (120×120 km, 20 km spacing)
- Divergence: `compute_divergence()` using finite-difference (replaces `uv2dv_cfd`)
- Relative vorticity: `compute_relative_vorticity()` finite-difference
- IKE: `compute_ike()` with cosine(lat)-weighted grid cell areas
- Graphics: `plot_tccen()` (Cartopy map), `plot_tchodo()` (polar hodograph), `plot_trend()` (time-series overlay)
- Variables computed: SHRD, SHTD, SHRS, SHTS, SHDC, SDDC, MSLP, PENV, VMAX, IKE34/50/64, U200, U20C, V20C, RHLO, RHMD, RHHI, R000, Z850, D200, DIVC, T000, CAPE, HLCY, TCCEN, TCHODO

---

### GPLOT_stats.ncl (5383 lines) — ✅ Done

**Purpose:** Top-level script; creates track/intensity forecast and verification graphics.
**Loads:** GPLOT_util + GPLOT_func + GPLOT_main
**Python target:** `python/GPLOT_stats.py` (1624 lines, 32 functions)

**Key design decisions:**
- Six-step structure preserved: ATCF locate → merge/read → guidance → trends → verification → status
- ATCF merging via Python sort+dedup (`_merge_atcf_files`) replaces shell `sort -s -t, -k3,3 ... -u`
- BDECK/ADECK parsing via `_read_atcf()` / `_read_bdeck()` (pure Python)
- Guidance graphics: `_make_track_map()` (Cartopy) + `_make_xy_plot()` (matplotlib)
- Intensity-coded track: `_intensity_color()` maps vmax→tcwinds1 colour
- Auto extent: `_auto_extent()` with 2:1 lon:lat aspect correction
- Trend graphics: last nTrend cycles, `_trend_cycle_colors()`, plus lifetime tracks
- MET-TC verification: `_run_verification()` shells out to tc_pairs / tc_stat; `_read_tcstat_summary()` parses output
- Products: TrackGuidance, TrackIntensityGuidance, IntensityGuidance, PressureGuidance, TrackTrend, AllTracks, IntensityTrend, AllIntensity, PressureTrend, Verification.TK_ERR, Verification.WIND

---

## Session Log

| Date | Work Done |
|---|---|
| pre-2026-02-25 | Cloned `support/HAFS_python` branch; converted `ECMWF_combine.ncl` and `calculate.ncl`; began `GPLOT_util_legacy.ncl` and `GPLOT_main.ncl` (session ended before writing output) |
| 2026-02-25 | Created journal; converted `GPLOT_util_legacy.ncl` → `gplot_util_legacy.py` (314 lines, 1 function); read and fully catalogued all 40 functions in `GPLOT_util.ncl`; writing of `gplot_util.py` interrupted |
| 2026-02-25 (session 3) | Completed `gplot_util.py` (2306 lines, 40 public functions); syntax verified via `ast.parse()`; committed |
| 2026-02-25 (session 4) | Converted `GPLOT_func.ncl` → `gplot_func.py` (1272 lines, 21 public functions); converted `GPLOT_main.ncl` → `gplot_main.py` (479 lines, 2 public functions); both syntax verified; committed |
| 2026-02-25 (session 5) | Converted `GPLOT_maps.ncl` → `GPLOT_maps.py` (1512 lines); full driver with all 7 overlay types (wind vectors, 2× streamlines, 2× contour lines, MSLP H/L markers, storm labels, titles); syntax verified; committed |
| 2026-02-26 (session 6) | Converted `GPLOT_ships.ncl` → `GPLOT_ships.py` (1616 lines, 27 functions); replaced SPH2CART Fortran external with haversine-based sph2cart() using scipy; all 26 SHIPS diagnostic variables + TCCEN/TCHODO graphics + trend plots; syntax verified; committed |
| 2026-02-26 (session 7) | Converted `GPLOT_stats.ncl` → `GPLOT_stats.py` (1624 lines, 32 functions); six-step structure preserved; Cartopy track maps, matplotlib intensity/pressure XY plots, trend/lifetime graphics, MET-TC verification wrapper; syntax verified; committed |

---

## Next Steps (in order)

1. ~~**Finish `GPLOT_util.ncl`**~~ ✅ Done
2. ~~**Convert `GPLOT_func.ncl`**~~ ✅ Done
3. ~~**Convert `GPLOT_main.ncl`**~~ ✅ Done
4. ~~**Convert `GPLOT_maps.ncl`**~~ ✅ Done
5. ~~**Convert `GPLOT_ships.ncl`**~~ ✅ Done
6. ~~**Convert `GPLOT_stats.ncl`**~~ ✅ Done
7. **Update shell/batch scripts** to call Python instead of NCL  ← **Next step**
