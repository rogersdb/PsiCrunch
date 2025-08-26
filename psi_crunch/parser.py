"""Parsers for EDL parameters and plane coefficients.

These lightweight helpers mimic the behaviour of the Fortran
parsers used in CrunchFlow's surface-complexation routines. They are
not intended to be a full replacement but provide enough
functionality for unit testing.
"""
from __future__ import annotations

from typing import Dict, Iterable, Tuple


def parse_planes_line(line: str) -> Dict[str, Tuple[float, float, float]]:
    """Parse a line containing the optional ``planes=`` syntax.

    Parameters
    ----------
    line:
        Input line describing a surface site.  An example is::

            ">FeOH 5.0 planes= 0.5 0.3 0.2"

    Returns
    -------
    dict
        Mapping with keys ``site``, ``z`` (the zeta potential) and
        ``planes`` containing a tuple of the three plane coefficients.
    """
    tokens = line.strip().split()
    if len(tokens) < 2:
        raise ValueError("line missing site or z value")
    site = tokens[0]
    z = float(tokens[1])
    planes = (None, None, None)
    if 'planes=' in tokens:
        idx = tokens.index('planes=') + 1
        try:
            planes = tuple(float(tokens[idx + i]) for i in range(3))
        except (IndexError, ValueError) as exc:  # pragma: no cover - defensive
            raise ValueError("invalid planes specification") from exc
    return {'site': site, 'z': z, 'planes': planes}


def parse_edl_block(lines: Iterable[str]) -> Dict[str, Tuple[float, float, float]]:
    """Parse an ``edl parameters`` block.

    Parameters
    ----------
    lines:
        Iterable over lines of an input file.  Only the portion between
        ``Begin edl parameters`` and ``End edl parameters`` is read.

    Returns
    -------
    dict
        Mapping from surface site names to ``(C1, C2, eps_r)`` tuples.
    """
    capture = False
    params: Dict[str, Tuple[float, float, float]] = {}
    for raw in lines:
        line = raw.strip()
        if not line:
            continue
        lower = line.lower()
        if lower.startswith('begin edl parameters'):
            capture = True
            continue
        if lower.startswith('end edl parameters'):
            break
        if capture:
            parts = line.split()
            if len(parts) >= 4:
                site = parts[0]
                values = tuple(float(p) for p in parts[1:4])
                params[site] = values
    return params
