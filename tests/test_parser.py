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


def test_parse_planes_line_with_comment():
    line = ">FeOH 5.0 planes= 0.5 0.3 0.2  # comment"
    result = parse_planes_line(line)
    assert result["planes"] == (0.5, 0.3, 0.2)


def test_parse_planes_line_missing_plane_values():
    line = ">FeOH 5.0 planes="
    with pytest.raises(ValueError):
        parse_planes_line(line)


def test_parse_planes_line_malformed_plane_spec():
    line = ">FeOH 5.0 planes= 0.5 0.3 not-a-number"
    with pytest.raises(ValueError):
        parse_planes_line(line)


def test_parse_planes_line_mixed_case_keyword():
    line = ">FeOH 5.0 PlAnEs= 0.5 0.3 0.2"
    result = parse_planes_line(line)
    assert result["planes"] == (0.5, 0.3, 0.2)


def test_parse_planes_line_exclamation_comment():
    line = ">FeOH 5.0 planes= 0.5 0.3 0.2 ! comment"
    result = parse_planes_line(line)
    assert result["planes"] == (0.5, 0.3, 0.2)


def test_parse_edl_block():
    lines = [
        "irrelevant line",
        "Begin edl parameters",
        "  >FeOH   1.0  0.2  78.5",
        "  >FeOH2  1.5  0.3  80.0",
        "End edl parameters",
    ]
    params, unmatched = parse_edl_block(lines)
    assert params[">FeOH"] == (1.0, 0.2, 78.5)
    assert params[">FeOH2"] == (1.5, 0.3, 80.0)
    assert len(params) == 2
    assert unmatched == []


def test_parse_edl_block_with_comments():
    lines = [
        "Begin edl parameters",
        "  # comment line",
        "  >FeOH   1.0  0.2  78.5  # trailing comment",
        "  >FeOH2  1.5  0.3  80.0  ! another",
        "End edl parameters  # end comment",
    ]
    params, unmatched = parse_edl_block(lines)
    assert params[">FeOH"] == (1.0, 0.2, 78.5)
    assert params[">FeOH2"] == (1.5, 0.3, 80.0)
    assert len(params) == 2
    assert unmatched == []


def test_parse_edl_block_case_insensitive_markers():
    lines = [
        "BEGIN EDL PARAMETERS",
        "  >FeOH   1.0  0.2  78.5",
        "EnD EdL PaRaMeTeRs",
    ]
    params, unmatched = parse_edl_block(lines)
    assert params[">FeOH"] == (1.0, 0.2, 78.5)
    assert len(params) == 1
    assert unmatched == []


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


def test_parse_edl_block_unmatched():
    lines = [
        "Begin edl parameters",
        "  >FeOH   1.0  0.2  78.5",
        "  >Unknown 1.1 0.2 78.5",
        "End edl parameters",
    ]
    with pytest.warns(RuntimeWarning):
        params, unmatched = parse_edl_block(lines, known_sites=[">FeOH"])
    assert ">FeOH" in params
    assert ">Unknown" not in params
    assert unmatched == [">Unknown"]


def test_parse_edl_block_missing_begin():
    lines = [
        "  >FeOH   1.0  0.2  78.5",
        "End edl parameters",
    ]
    params, unmatched = parse_edl_block(lines)
    assert params == {}
    assert unmatched == []


def test_parse_edl_block_bang_comment_line():
    lines = [
        "Begin edl parameters",
        "  ! an exclamation comment",
        "  >FeOH   1.0  0.2  78.5 ! trailing",
        "End edl parameters",
    ]
    params, unmatched = parse_edl_block(lines)
    assert params[">FeOH"] == (1.0, 0.2, 78.5)
    assert unmatched == []

