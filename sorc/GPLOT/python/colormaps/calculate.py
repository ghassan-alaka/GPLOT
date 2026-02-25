#!/usr/bin/env python
"""
calculate.py

Data transformation utility:
  - Reads temp.dat
  - Extracts columns 4-6 as integers
  - Reorganizes into a 15x3 array (reversed column groups)
  - Writes formatted output (3 integers, width 4 each)

Original NCL: sorc/GPLOT/ncl/colormaps/calculate.ncl
"""

import numpy as np


def main():
    with open("temp.dat", "r") as f:
        lines = [line for line in f.readlines() if line.strip()]

    # Extract columns 4-6 (0-indexed char positions 4:7) as integers
    data = np.array([int(line[4:7]) for line in lines])

    # Reorganize into 15x3 matrix with reversed column groups
    out = np.zeros((15, 3), dtype=int)
    for i in range(15):
        out[i, 0] = data[i + 30]
        out[i, 1] = data[i + 15]
        out[i, 2] = data[i]

    # Print 3I4 formatted output (each integer width 4)
    for row in out:
        print("{:4d}{:4d}{:4d}".format(row[0], row[1], row[2]))


if __name__ == "__main__":
    main()
