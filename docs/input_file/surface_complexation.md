## SURFACE_COMPLEXATION

Surface complexation follows the approach outlined in Dzombak and Morel
(1990), with either a double layer or non-electrostatic model possible.
Currently, complexation must be on a specific mineral, so a valid
mineral name (listed in the **MINERALS** keyword block) must be given,
for example:

    >FeOH_strong on Fe(OH)3
    >FeOH_weak on Fe(OH)3

To specify a non-electrostatic model, the mineral name should be
followed by the keyword *--no_edl*, for example:

    >FeOH_strong on Fe(OH)3 -no_edl
    >FeOH_weak on Fe(OH)3   -no_edl

The capability for surface complexation on the bulk material will be
added soon.

### EDL Parameters

Electrostatic double-layer properties for surface sites are specified in an
``edl parameters`` block::

    Begin edl parameters
      >FeOH   1.0  0.2  78.5
    End edl parameters

Each line defines the site name followed by ``C1``, ``C2`` and the dielectric
constant ``eps_r``. Entries whose site names do not match any defined surface
complex are ignored and a warning is issued so that unmatched sites can be
identified and corrected.
