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

    add = rows["i32.add"]
    assert add["mayTrap"] == "false", add
    assert add["pops"] == "2" and add["pushes"] == "1", add

    div = rows["i32.div_s"]
    assert div["mayTrap"] == "true", div
    assert div["pops"] == "2" and div["pushes"] == "1", div

    tee = rows["local.tee"]
    assert "local(x)" in regions(tee["writes"]), tee
    assert tee["pops"] == "1" and tee["pushes"] == "1", tee
    assert tee["mayTrap"] == "false", tee

    fill = rows["memory.fill"]
    assert "mem" in regions(fill["writes"]), fill
    assert fill["pops"] == "3" and fill["pushes"] == "0", fill
    assert fill["mayTrap"] == "true", fill

    call = rows["call"]
    assert call["mayDiverge"] == "true", call
    assert call["mayTrap"] == "true", call
    assert regions(call["reads"]) == {"glob(*)", "mem", "table"}, call
    assert regions(call["writes"]) == {"glob(*)", "mem", "table"}, call
    assert call["pops"] == "*" and call["pushes"] == "*", call

    print("effects invariants OK")


if __name__ == "__main__":
    main(sys.argv[1])
