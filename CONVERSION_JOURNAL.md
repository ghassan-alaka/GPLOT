# GPLOT NCL → Python Conversion Journal

**Project:** Convert all NCL scripts in `sorc/GPLOT/ncl/` to Python equivalents in `sorc/GPLOT/python/`
**Started:** 2026-02-25
**Last Updated:** 2026-02-25

---

## Quick Status

| NCL File | Lines | Python Target | Status | Notes |
|---|---|---|---|---|
| `ECMWF_combine.ncl` | 192 | `python/ECMWF_combine.py` | ✅ Done | |
| `colormaps/calculate.ncl` | — | `python/colormaps/calculate.py` | ✅ Done | |
| `GPLOT_util_legacy.ncl` | 314 | `python/modules/gplot_util_legacy.py` | 🔄 In Progress | Session ended before output written |
| `GPLOT_main.ncl` | 842 | `python/modules/gplot_main.py` | 🔄 In Progress | Session ended before output written |
| `GPLOT_func.ncl` | 2227 | `python/modules/gplot_func.py` | ⏳ Pending | |
| `GPLOT_util.ncl` | 3673 | `python/modules/gplot_util.py` | ⏳ Pending | |
| `GPLOT_ships.ncl` | 3072 | `python/GPLOT_ships.py` | ⏳ Pending | Top-level script (no named functions) |
| `GPLOT_stats.ncl` | 5383 | `python/GPLOT_stats.py` | ⏳ Pending | Top-level script (no named functions) |
| `GPLOT_maps.ncl` | 3269 | `python/GPLOT_maps.py` | ⏳ Pending | Top-level script (no named functions) |
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

### GPLOT_util.ncl (3673 lines) — ⏳ Pending

**Purpose:** Core utility library; lowest-level, no NCL deps.

**Functions (26+ total):**
- `load_constants` — physical constants dict
- `allMasters` — list of model names by type
- `arrow` — draw arrow on plot
- `basinCodes` — map basin name↔code
- `calcTheta` — potential temperature
- `changeTimeFmt` — reformat time strings
- `chkCmdInputs` — validate command-line args
- `circle_ll` — draw lat/lon circle on map
- `defineCMAP_fill` / `defineCMAP_name` — colormap selection by variable
- `defineLevels` — contour levels by variable/level
- `filter121` — 1-2-1 smoother (replaces Fortran filter121.so)
- `findVarName` — variable name mapper (canonical version; legacy version in util_legacy)
- `GenPlotRes` — generate plot resource block
- `getAutoDir` — auto-detect input directory
- `getDmnBds` / `getDmnInfo` — domain bounds/info lookup
- `getExptInfo` — parse experiment metadata
- `getIDIR` / `getFileTag` — build input directory/filename paths
- (+ more, file is 3673 lines)

**Python target:** `python/modules/gplot_util.py`
**Note:** Very large; may be worth splitting into sub-modules (e.g., `gplot_util_plot.py`, `gplot_util_io.py`) after initial port.

---

### GPLOT_maps.ncl (3269 lines) — ⏳ Pending

**Purpose:** Top-level script; produces 2D map graphics from model output.
**Loads:** GPLOT_util + GPLOT_func + GPLOT_main
**External:** `sph2cart.so` Fortran lib (spherical→Cartesian)
**Python target:** `python/GPLOT_maps.py`
**Note:** No named NCL functions; monolithic `begin...end` block. Convert to `main()`.

---

### GPLOT_ships.ncl (3072 lines) — ⏳ Pending

**Purpose:** Top-level script; extracts SHIPS/LSDIAG diagnostic variables and writes `.DAT` files + graphics.
**Loads:** GPLOT_util + GPLOT_func + GPLOT_main
**External:** `sph2cart.so`
**Python target:** `python/GPLOT_ships.py`

---

### GPLOT_stats.ncl (5383 lines) — ⏳ Pending

**Purpose:** Top-level script; creates track/intensity forecast and verification graphics.
**Loads:** GPLOT_util + GPLOT_func + GPLOT_main
**Python target:** `python/GPLOT_stats.py`
**Note:** Largest file. May be worth splitting into sub-scripts by product type after initial port.

---

## Session Log

| Date | Work Done |
|---|---|
| pre-2026-02-25 | Cloned `support/HAFS_python` branch; converted `ECMWF_combine.ncl` and `calculate.ncl`; began `GPLOT_util_legacy.ncl` and `GPLOT_main.ncl` (session ended before writing output) |
| 2026-02-25 | Created this journal; confirmed status of all files; ready to resume |

---

## Next Steps (in order)

1. **Resume `GPLOT_util_legacy.ncl`** → write `python/modules/gplot_util_legacy.py`
2. **Convert `GPLOT_util.ncl`** → write `python/modules/gplot_util.py` (dependency for all others)
3. **Convert `GPLOT_func.ncl`** → write `python/modules/gplot_func.py`
4. **Resume `GPLOT_main.ncl`** → write `python/modules/gplot_main.py`
5. **Convert `GPLOT_maps.ncl`** → write `python/GPLOT_maps.py`
6. **Convert `GPLOT_ships.ncl`** → write `python/GPLOT_ships.py`
7. **Convert `GPLOT_stats.ncl`** → write `python/GPLOT_stats.py`
8. **Update shell/batch scripts** to call Python instead of NCL
