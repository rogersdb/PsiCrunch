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

