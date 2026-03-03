# GPLOT Python Conversion Bug Fix Work Log

## Session: 2026-03-03

### Overview
Two bugs found and fixed in the NCL-to-Python conversion of GPLOT_maps.py
and GPLOT_ships.py.

---

### Bug 1: GPLOT_maps.py - "No graphics turned on in this namelist"

**File:** `sorc/GPLOT/python/GPLOT_maps.py`
**Function:** `_read_gfx_namelist()` (line ~312)

**Root Cause:**
The namelist parser split lines using `split("\t")` (tab-only), but the
graphics namelist files (e.g., `parm/namelist.maps.default.Tier1`) use
**spaces** for column separation, not tabs. Since there were no tabs in
the files, `split("\t")` returned each entire line as a single element.
This caused `headers.index("PLOT_ON")` to raise `ValueError`, and the
fallback returned empty strings for every column, making all `What2Plot`
entries `False`.

The original NCL code used `str_split(data(0), "\t ")` which splits on
both tabs and spaces.

**Fix:**
- Changed `lines[0].split("\t")` to `lines[0].split()` (split on any
  whitespace).
- Changed the `_col()` helper to also use `split()` for data lines.
- Added filtering to skip blank lines (trailing empty lines in namelist
  files).

**Verification:**
Tested against `parm/namelist.maps.default.Tier1` and
`parm/namelist.maps.default` — all `PLOT_ON=True` entries correctly
detected, `any(What2Plot)` returns `True`.

---

### Bug 2: GPLOT_ships.py - "Environment variable DSOURCE is not set"

**File:** `sorc/GPLOT/python/GPLOT_ships.py`
**Function:** `main()` (line ~863)

**Root Cause (two issues):**

1. **Order of operations:** DSOURCE was read only from `os.environ` and
   checked as a required variable **before** the master namelist was read.
   In the NCL code, the master namelist is read first, and DSOURCE falls
   back to the namelist when the env var isn't set:
   ```ncl
   if(.not.isvar("DSOURCE"))then  DSOURCE = NML@DSOURCE  end if
   ```

2. **Wrong accessor method:** After reading the namelist, the code used
   `NML.get("KEY", default)` (dict syntax), but `read_master_namelist()`
   returns a `SimpleNamespace`, not a dict. This would cause an
   `AttributeError` whenever the namelist was successfully read.
   (GPLOT_maps.py correctly uses `getattr(NML, key, default)`.)

**Fix:**
- Moved master namelist reading to **before** the required-variable check.
- Added a `_nml()` helper (matching the pattern in GPLOT_maps.py) that
  checks env vars first, then falls back to namelist attributes using
  `getattr()`.
- Also improved the master namelist path search to try both the raw path
  and a `parm/` subdirectory path, matching how GPLOT_maps.py handles it.
- Updated the error message to mention both env var and namelist as sources.

---

### Files Modified
- `sorc/GPLOT/python/GPLOT_maps.py` — Fixed `_read_gfx_namelist()` parser
- `sorc/GPLOT/python/GPLOT_ships.py` — Fixed variable initialization order
  and namelist access pattern

### Status
- Both fixes implemented and basic verification done.
- Full integration testing recommended on actual model output data.

### Next Steps (if needed)
- Run end-to-end tests with actual HAFS model output
- Check other converted Python scripts for similar patterns:
  - `GPLOT_stats.py`
  - `plot_ocean_maps.py`
  - `plot_airsea_pbl.py`
  - `polar_cylindrical_structure.py`
