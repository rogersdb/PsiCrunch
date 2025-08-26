import pytest
from psi_crunch import parse_edl_block, parse_planes_line


def test_parse_planes_line():
    line = ">FeOH 5.0 planes= 0.5 0.3 0.2"
    result = parse_planes_line(line)
    assert result["site"] == ">FeOH"
    assert result["z"] == 5.0
    assert result["planes"] == (0.5, 0.3, 0.2)


def test_parse_planes_line_uppercase():
    line = ">FeOH 5.0 PLANES= 0.5 0.3 0.2"
    result = parse_planes_line(line)
    assert result["planes"] == (0.5, 0.3, 0.2)


def test_parse_planes_line_attached_value():
    line = ">FeOH 5.0 planes=0.5 0.3 0.2"
    result = parse_planes_line(line)
    assert result["planes"] == (0.5, 0.3, 0.2)


def test_parse_planes_line_default_planes():
    """Lines without a ``planes=`` specification yield ``None``."""
    line = ">FeOH 5.0"
    result = parse_planes_line(line)
    assert result["planes"] is None


def test_parse_edl_block():
    lines = [
        "irrelevant line",
        "Begin edl parameters",
        "  >FeOH   1.0  0.2  78.5",
        "  >FeOH2  1.5  0.3  80.0",
        "End edl parameters",
    ]
    params = parse_edl_block(lines)
    assert params[">FeOH"] == (1.0, 0.2, 78.5)
    assert params[">FeOH2"] == (1.5, 0.3, 80.0)
    assert len(params) == 2


def test_parse_edl_block_missing_end():
    lines = [
        "Begin edl parameters",
        "  >FeOH   1.0  0.2  78.5",
    ]
    with pytest.raises(ValueError):
        parse_edl_block(lines)


def test_parse_edl_block_malformed_line():
    lines = [
        "Begin edl parameters",
        "  >FeOH   1.0  bad  78.5",
        "End edl parameters",
    ]
    with pytest.raises(ValueError):
        parse_edl_block(lines)

