#!/usr/bin/env python3
"""Patch is-encounter-reportable in the trimmed KAR with an alternate expression."""
import json
import sys
from pathlib import Path

KAR = Path("src/test/resources/Bsa/Scenarios/kars/rulefilters/eRSD-RuleFilter-bundle.json")


def walk_actions(actions, target_id, new_expr):
    for action in actions:
        if action.get("id") == target_id:
            for cond in action.get("condition", []):
                cond["expression"]["expression"] = new_expr
            return True
        if "action" in action:
            if walk_actions(action["action"], target_id, new_expr):
                return True
    return False


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: probe_expression.py '<fhirpath expression>'")
    new_expr = sys.argv[1]
    bundle = json.loads(KAR.read_text())
    for entry in bundle["entry"]:
        if entry["resource"].get("resourceType") == "PlanDefinition":
            if walk_actions(entry["resource"].get("action", []), "is-encounter-reportable", new_expr):
                print(f"Patched is-encounter-reportable expression ({len(new_expr)} chars)")
                break
    else:
        sys.exit("is-encounter-reportable not found")
    KAR.write_text(json.dumps(bundle, indent=2))


if __name__ == "__main__":
    main()
