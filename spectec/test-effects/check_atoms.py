#!/usr/bin/env python3
"""Invariants for --atom-sem closed transformers."""
import json
import sys


def src_pop0(src: dict) -> bool:
    return src == {"pop": 0}


def src_local_x(src: dict) -> bool:
    return src == {"region": "local", "idx": "x"}


def src_glob_x(src: dict) -> bool:
    return src == {"region": "glob", "idx": "x"}


def write_local_pop0(w: dict) -> bool:
    return (
        w.get("region") == "local"
        and w.get("idx") == "x"
        and src_pop0(w.get("from", {}))
    )


def write_glob_pop0(w: dict) -> bool:
    return (
        w.get("region") == "glob"
        and w.get("idx") == "x"
        and src_pop0(w.get("from", {}))
    )


def main(path: str) -> None:
    with open(path) as f:
        data = json.load(f)
    rows = {a["instruction"]: a for a in data["atoms"]}

    required = [
        "drop",
        "local.get",
        "local.set",
        "local.tee",
        "nop",
        "global.get",
        "global.set",
    ]
    missing = [n for n in required if n not in rows]
    assert not missing, f"missing atoms: {missing}"

    drop = rows["drop"]
    assert drop["pop"] == 1 and drop["push"] == [] and drop["writes"] == [], drop

    get = rows["local.get"]
    assert get["pop"] == 0, get
    assert len(get["push"]) == 1 and src_local_x(get["push"][0]), get
    assert get["writes"] == [], get

    set_ = rows["local.set"]
    assert set_["pop"] == 1 and set_["push"] == [], set_
    assert len(set_["writes"]) == 1 and write_local_pop0(set_["writes"][0]), set_

    tee = rows["local.tee"]
    assert tee["pop"] == 1, tee
    assert len(tee["push"]) == 1 and src_pop0(tee["push"][0]), tee
    assert len(tee["writes"]) == 1 and write_local_pop0(tee["writes"][0]), tee

    nop = rows["nop"]
    assert nop["pop"] == 0 and nop["push"] == [] and nop["writes"] == [], nop

    gg = rows["global.get"]
    assert gg["pop"] == 0, gg
    assert len(gg["push"]) == 1 and src_glob_x(gg["push"][0]), gg
    assert gg["writes"] == [], gg

    gs = rows["global.set"]
    assert gs["pop"] == 1 and gs["push"] == [], gs
    assert len(gs["writes"]) == 1 and write_glob_pop0(gs["writes"][0]), gs

    print("atom-sem invariants OK")


if __name__ == "__main__":
    main(sys.argv[1])
