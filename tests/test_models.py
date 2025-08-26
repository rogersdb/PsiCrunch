import pytest

from psi_crunch import constant_capacitance, triple_layer


def test_ccm_vs_tlm_ionic_strength_sweep():
    strengths = [1e-3, 1e-2, 1e-1]
    surface_charge = 1.0
    ccm_results = [
        constant_capacitance(i, surface_charge, capacitance=1.0)
        for i in strengths
    ]
    tlm_results = [
        triple_layer(i, surface_charge, C1=1.0, C2=2.0)
        for i in strengths
    ]
    # CCM is independent of ionic strength
    assert ccm_results == [ccm_results[0]] * len(strengths)
    # TLM potential decreases with ionic strength and is lower than CCM
    assert tlm_results[0] > tlm_results[-1]
    assert all(t < c for t, c in zip(tlm_results, ccm_results))


def test_constant_capacitance_invalid_capacitance():
    for bad_cap in [0.0, -1.0]:
        with pytest.raises(ValueError, match="capacitance must be positive"):
            constant_capacitance(1e-3, 1.0, capacitance=bad_cap)


def test_triple_layer_invalid_coefficients():
    for bad_c1 in [0.0, -1.0]:
        with pytest.raises(ValueError, match="C1 must be positive"):
            triple_layer(1e-3, 1.0, C1=bad_c1, C2=1.0)
    for bad_c2 in [0.0, -1.0]:
        with pytest.raises(ValueError, match="C2 must be positive"):
            triple_layer(1e-3, 1.0, C1=1.0, C2=bad_c2)

