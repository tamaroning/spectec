#!/usr/bin/env python3
"""Invariants for --effects CSV (loop/jump-free summaries)."""
import csv
import sys

CONTROL = {"block", "loop", "if", "br", "br_if", "br_table", "return"}


def regions(cell: str) -> set[str]:
    if not cell:
        return set()
    return set(cell.split(";"))


def main(path: str) -> None:
    with open(path, newline="") as f:
        rows = {r["instruction"]: r for r in csv.DictReader(f)}

    names = set(rows)
    overlap = names & CONTROL
    assert not overlap, f"control instructions present: {overlap}"

    for col in ("mayTrap", "mayThrow", "mayDiverge", "mayNondet"):
        assert col in next(iter(rows.values())), f"missing column {col}"

    add = rows["i32.add"]
    assert add["mayTrap"] == "false", add
    assert add["mayThrow"] == "false", add
    assert add["mayNondet"] == "false", add
    assert add["pops"] == "2" and add["pushes"] == "1", add

    div = rows["i32.div_s"]
    assert div["mayTrap"] == "true", div
    assert div["pops"] == "2" and div["pushes"] == "1", div

    tee = rows["local.tee"]
    assert "local(x)" in regions(tee["writes"]), tee
    assert tee["pops"] == "1" and tee["pushes"] == "1", tee
    assert tee["mayTrap"] == "false", tee

    fill = rows["memory.fill"]
    assert any(r.startswith("mem(") for r in regions(fill["writes"])), fill
    assert fill["pops"] == "3" and fill["pushes"] == "0", fill
    assert fill["mayTrap"] == "true", fill

    call = rows["call"]
    assert call["mayDiverge"] == "true", call
    assert call["mayTrap"] == "true", call
    assert call["mayThrow"] == "true", call
    assert call["mayNondet"] == "true", call
    assert regions(call["reads"]) == {
        "glob(*)",
        "mem(*)",
        "table",
        "elem",
        "data",
        "struct",
        "array",
        "exn",
    }, call
    assert regions(call["writes"]) == regions(call["reads"]), call
    assert call["pops"] == "*" and call["pushes"] == "*", call

    if "memory.grow" in rows:
        grow = rows["memory.grow"]
        assert grow["mayTrap"] == "false", grow
        assert grow["mayNondet"] == "true", grow
        assert any(r.startswith("mem(") for r in regions(grow["reads"])), grow
        assert any(r.startswith("mem(") for r in regions(grow["writes"])), grow

    if "data.drop" in rows:
        drop = rows["data.drop"]
        assert "data" in regions(drop["writes"]), drop

    if "struct.get" in rows:
        sg = rows["struct.get"]
        assert "struct" in regions(sg["reads"]), sg
        assert sg["mayTrap"] == "true", sg

    if "struct.new" in rows:
        sn = rows["struct.new"]
        assert regions(sn["reads"]) == set(), sn
        assert regions(sn["writes"]) == set(), sn
        assert sn["mayNondet"] == "true", sn

    if "throw" in rows:
        throw = rows["throw"]
        assert "exn" in regions(throw["writes"]), throw
        assert throw["mayThrow"] == "true", throw

    print("effects invariants OK")


if __name__ == "__main__":
    main(sys.argv[1])
