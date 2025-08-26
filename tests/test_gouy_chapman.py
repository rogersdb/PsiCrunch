import math
from psi_crunch import gouy_chapman_potential


def test_gouy_chapman_potential_basic():
    psi = gouy_chapman_potential(0.01, surface_charge=1.0)
    assert math.isclose(psi, 10.0)


def test_gouy_chapman_requires_positive_ionic_strength():
    try:
        gouy_chapman_potential(0.0, 1.0)
    except ValueError:
        pass
    else:
        raise AssertionError("expected ValueError for zero ionic strength")


