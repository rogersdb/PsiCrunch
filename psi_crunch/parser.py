"""Parsers for EDL parameters and plane coefficients.

These lightweight helpers mimic the behaviour of the Fortran
parsers used in CrunchFlow's surface-complexation routines. They are
not intended to be a full replacement but provide enough
functionality for unit testing.
"""
from __future__ import annotations

from typing import Dict, Iterable, Optional, Tuple


def parse_planes_line(line: str) -> Dict[str, Optional[Tuple[float, float, float]]]:
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
        ``planes`` containing a tuple of the three plane coefficients if
        provided.  When the ``planes=`` specification is absent, the
        ``planes`` entry is ``None``.
    """
    tokens = line.strip().split()
    if len(tokens) < 2:
        raise ValueError("line missing site or z value")
    site = tokens[0]
    z = float(tokens[1])
    planes: Optional[Tuple[float, float, float]] = None

    tokens_lower = [t.lower() for t in tokens]
    for idx, tok in enumerate(tokens_lower):
        if tok == 'planes=':
            start = idx + 1
            try:
                planes = tuple(float(tokens[start + i]) for i in range(3))
            except (IndexError, ValueError) as exc:  # pragma: no cover - defensive
                raise ValueError("invalid planes specification") from exc
            break
        elif tok.startswith('planes='):
            first = tokens[idx][len('planes='):]
            try:
                planes = tuple(
                    float(val) for val in [first] + tokens[idx + 1 : idx + 3]
                )
            except (IndexError, ValueError) as exc:  # pragma: no cover - defensive
                raise ValueError("invalid planes specification") from exc
            break

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

    Raises
    ------
    ValueError
        If the block is not terminated by ``End edl parameters`` or if any
        line inside the block does not contain exactly three numeric values.
    """
    in_block = False
    params: Dict[str, Tuple[float, float, float]] = {}
    for raw in lines:
        line = raw.strip()
        if not line:
            continue
        lower = line.lower()
        if lower.startswith('begin edl parameters'):
            in_block = True
            continue
        if lower.startswith('end edl parameters'):
            in_block = False
            break
        if in_block:
            parts = line.split()
            if len(parts) != 4:
                raise ValueError(f"malformed line in edl parameters: {line!r}")
            site = parts[0]
            try:
                values = tuple(float(p) for p in parts[1:4])
            except ValueError as exc:
                raise ValueError(f"malformed line in edl parameters: {line!r}") from exc
            params[site] = values
    if in_block:
        raise ValueError("edl parameters block missing 'End edl parameters'")
    return params
