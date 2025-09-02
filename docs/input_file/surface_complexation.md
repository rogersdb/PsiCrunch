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

### Input file

The **SURFACE_COMPLEXATION** block lists the surface sites included in a
problem and the mineral on which each site resides:

```
SURFACE_COMPLEXATION
  >FeOH_strong on Fe(OH)3
  >FeOH_weak   on Fe(OH)3
End
```

Adding ``--no_edl`` after the mineral name disables the electrical double
layer for that particular site.

### Database entries for triple layer models

Triple layer models require additional information in the database file.
Each surface complex and its associated charge are listed in a
``surface complexation parameters`` block.  Optional plane fractions may
be supplied using the ``planes=`` keyword to distribute the charge among
the 0, 1 and 2 planes.  The three coefficients should sum to unity; when
``planes=`` is omitted, all coefficients default to zero.

```
Begin surface complexation parameters
>FeOH    0.0  planes= 0.5 0.3 0.2
>FeOH2   1.0  planes= 0.3 0.4 0.3
>FeO-   -1.0  planes= 0.5 0.3 0.2
>FeOH2+  1.0  planes= 0.3 0.4 0.3
End surface complexation parameters
```

Electrostatic double-layer properties are specified in an ``edl
parameters`` block that lists the site name followed by ``C1``, ``C2`` and
the relative dielectric constant ``eps_r``:

```
Begin edl parameters
  >FeOH   1.0  0.2  78.5
  >FeOH2  1.5  0.3  80.0
End edl parameters
```

Entries whose site names do not match any defined surface complex are
ignored and a warning is issued so that unmatched sites can be identified
and corrected.

A concise demonstration of the input and database syntax is provided in
the example files
[examples/tlm_demo.in](../../examples/tlm_demo.in) and
[examples/tlm_demo.dbs](../../examples/tlm_demo.dbs).
