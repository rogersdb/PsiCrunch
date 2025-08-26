"""Simple electrostatic models used for testing."""
from __future__ import annotations

import math
from typing import Iterable, List


def gouy_chapman_potential(ionic_strength: float, surface_charge: float) -> float:
    """Return a toy Gouy--Chapman potential in volts.

    The implementation is intentionally lightweight; it simply returns
    ``surface_charge / sqrt(ionic_strength)`` which captures the
    qualitative trend of decreasing potential with increasing ionic
    strength.
    """
    if ionic_strength <= 0:
        raise ValueError("ionic strength must be positive")
    return surface_charge / math.sqrt(ionic_strength)


def constant_capacitance(ionic_strength: float, surface_charge: float, capacitance: float) -> float:
    """Constant Capacitance Model (CCM) potential."""
    if capacitance <= 0:
        raise ValueError("capacitance must be positive")
    return surface_charge / capacitance


def triple_layer(ionic_strength: float, surface_charge: float, C1: float, C2: float) -> float:
    """Triple Layer Model (TLM) potential.

    A toy implementation where the effective capacitance increases with
    ionic strength through the Stern layer capacitor ``C2``.
    """
    if C1 <= 0:
        raise ValueError("C1 must be positive")
    if C2 <= 0:
        raise ValueError("C2 must be positive")
    return surface_charge / (C1 + C2 * ionic_strength)
