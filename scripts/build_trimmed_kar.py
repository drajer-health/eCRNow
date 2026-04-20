#!/usr/bin/env python3
"""
Build a trimmed KAR bundle for Phase 1 integration tests.

Inputs:
  - v3 spec bundle (source of VSAC ValueSets and 6 grouper ValueSets)
  - Phase 1 PlanDefinition
  - existing eRSD-RuleFilter-bundle.json (source of RuleFilters + FHIRHelpers Libraries)

Output:
  - src/test/resources/Bsa/Scenarios/kars/rulefilters/eRSD-RuleFilter-bundle.json

Policy:
  * Keep only VSACs that cover Phase 1 scenario codes.
  * Keep 6 groupers; rewrite each compose.include[].valueSet list to reference
    only kept VSACs, stripping the |version suffix so
    ApplicationUtils.getValueSetByIds (exact equalsIgnoreCase) can match.
  * Rewrite Phase 1 PD's RuleFilters library ref from 3.0.0 -> 1.0.0 to match
    the shipped library.
"""
import json
import re
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
V3_BUNDLE = Path(
    "/Users/christopherschuler/Documents/workspace/aphl/aphl-ersd-specifications-v3/"
    "input/resources/bundle/eRSDv3_specification_bundle.json"
)
PHASE1_PD = Path(
    "/Users/christopherschuler/Documents/workspace/aphl/aphl-ersd-specifications-v3/"
    "input/resources/plandefinition/plandefinition-us-ecr-specification-phase1.json"
)
EXISTING_KAR = REPO_ROOT / "src/test/resources/Bsa/Scenarios/kars/rulefilters/eRSD-RuleFilter-bundle.json"
# Backup of the original pre-trim KAR, which still carries VSACs 1034/1035
# (Phase 1 exclusion value sets not present in the v3 spec bundle).
SUPPLEMENTAL_KAR = Path("/tmp/eRSD-RuleFilter-bundle.backup.json")
OUTPUT = EXISTING_KAR

# VSACs that cover every code used by any phase1-* scenario (trigger-side),
# plus the two "value-side" VSACs that Phase 1 FHIRPath references directly
# for negative/indeterminate result exclusion.
KEEP_VSACS = {
    "2.16.840.1.113762.1.4.1146.238",   # Chlamydia lab LOINC 14461-8  (lrtc)
    "2.16.840.1.113762.1.4.1146.245",   # Gonorrhea lab LOINC 698-1    (lrtc, ostc)
    "2.16.840.1.113762.1.4.1146.1124",  # COVID-19 SNOMED 840539006    (dxtc)
    "2.16.840.1.113762.1.4.1146.1601",  # HIV ARV Boosters — used by combo-med test  (mrtc)
    "2.16.840.1.113762.1.4.1146.1034",  # Negative/Not-detected result values  (phase1 exclusion)
    "2.16.840.1.113762.1.4.1146.1035",  # Indeterminate/Equivocal result values (phase1 exclusion)
}

GROUPER_URLS = {
    "http://ersd.aimsplatform.org/fhir/ValueSet/dxtc",
    "http://ersd.aimsplatform.org/fhir/ValueSet/lrtc",
    "http://ersd.aimsplatform.org/fhir/ValueSet/ostc",
    "http://ersd.aimsplatform.org/fhir/ValueSet/lotc",
    "http://ersd.aimsplatform.org/fhir/ValueSet/mrtc",
    "http://ersd.aimsplatform.org/fhir/ValueSet/sdtc",
}


def load(path):
    with path.open() as f:
        return json.load(f)


def strip_canonical_version(canon):
    return re.sub(r"\|[^|]+$", "", canon)


def vsac_oid_from_url(url):
    m = re.search(r"2\.16\.840\.1\.113762\.1\.4\.1146\.\d+", url)
    return m.group(0) if m else None


def main():
    v3 = load(V3_BUNDLE)
    existing = load(EXISTING_KAR)
    phase1_pd = load(PHASE1_PD)

    # Rewrite Phase 1 PD library refs 3.0.0 -> 1.0.0 and the RuleFilters URL to
    # match the example.org URL used by the bundled library.
    def rewrite_library_refs(obj):
        if isinstance(obj, dict):
            for k, v in list(obj.items()):
                if isinstance(v, str):
                    if v.startswith("http://example.org/fhir/Library/RuleFilters|3.0.0"):
                        obj[k] = "http://example.org/fhir/Library/RuleFilters|1.0.0"
                    elif v.startswith("Library/RuleFilters|3.0.0"):
                        obj[k] = "Library/RuleFilters|1.0.0"
                else:
                    rewrite_library_refs(v)
        elif isinstance(obj, list):
            for i, v in enumerate(obj):
                if isinstance(v, str):
                    if v.startswith("http://example.org/fhir/Library/RuleFilters|3.0.0"):
                        obj[i] = "http://example.org/fhir/Library/RuleFilters|1.0.0"
                    elif v.startswith("Library/RuleFilters|3.0.0"):
                        obj[i] = "Library/RuleFilters|1.0.0"
                else:
                    rewrite_library_refs(v)

    rewrite_library_refs(phase1_pd)

    # Keep RuleFilters + FHIRHelpers from the existing KAR (versions match the
    # refs we just rewrote).
    libraries = [
        e for e in existing["entry"]
        if e["resource"].get("resourceType") == "Library"
        and e["resource"].get("id") in {"RuleFilters", "FHIRHelpers"}
    ]
    if len(libraries) != 2:
        raise RuntimeError(f"expected 2 libraries from existing KAR, got {len(libraries)}")

    # Pull groupers from v3 and trim their include.valueSet lists.
    keep_vsac_urls = {
        f"http://cts.nlm.nih.gov/fhir/ValueSet/{oid}" for oid in KEEP_VSACS
    }

    groupers = []
    for entry in v3["entry"]:
        res = entry["resource"]
        if res.get("resourceType") != "ValueSet":
            continue
        if res.get("url") not in GROUPER_URLS:
            continue

        # Trim include[].valueSet to kept VSACs and strip |version suffixes.
        new_include = []
        for inc in res.get("compose", {}).get("include", []):
            vset = inc.get("valueSet", [])
            kept = []
            for canon in vset:
                stripped = strip_canonical_version(canon)
                if stripped in keep_vsac_urls:
                    kept.append(stripped)
            if kept:
                new_inc = dict(inc)
                new_inc["valueSet"] = kept
                new_include.append(new_inc)

        res["compose"]["include"] = new_include
        # Strip the version suffix from the grouper's own URL so tests that
        # reference the bare canonical continue to work.
        groupers.append({"resource": res})

    if len(groupers) != 6:
        raise RuntimeError(f"expected 6 groupers, got {len(groupers)}")

    # Pull the kept VSACs themselves — first from v3, then fall back to the
    # existing KAR for any VSAC v3 doesn't carry (e.g. 1034/1035, which are
    # supplemental Phase 1 exclusion value sets not part of the standard eRSD).
    kept_value_sets = []
    found_urls = set()
    for entry in v3["entry"]:
        res = entry["resource"]
        if res.get("resourceType") != "ValueSet":
            continue
        if res.get("url") not in keep_vsac_urls:
            continue
        kept_value_sets.append({"resource": res})
        found_urls.add(res["url"])

    supplemental = load(SUPPLEMENTAL_KAR) if SUPPLEMENTAL_KAR.exists() else None
    if supplemental:
        for url in keep_vsac_urls - found_urls:
            for entry in supplemental["entry"]:
                res = entry["resource"]
                if res.get("resourceType") == "ValueSet" and res.get("url") == url:
                    kept_value_sets.append({"resource": res})
                    found_urls.add(url)
                    break

    missing = keep_vsac_urls - found_urls
    if missing:
        raise RuntimeError(f"VSACs not found in either v3 or existing KAR: {missing}")

    # Summary
    print(f"Libraries: {[e['resource']['id'] for e in libraries]}")
    print(f"Groupers:  {[e['resource']['id'] for e in groupers]}")
    print(f"VSACs ({len(kept_value_sets)}):")
    for e in kept_value_sets:
        r = e["resource"]
        code_count = sum(
            len(inc.get("concept", [])) for inc in r.get("compose", {}).get("include", [])
        )
        print(f"  {r['id']}  codes={code_count}  {r.get('title', '')}")

    # Assemble output bundle (keep the existing bundle's structure/id).
    output = {
        "resourceType": "Bundle",
        "id": existing.get("id", "eRSD-RuleFilter"),
        "type": existing.get("type", "collection"),
        "entry": (
            [{"resource": phase1_pd}]
            + libraries
            + groupers
            + kept_value_sets
        ),
    }

    with OUTPUT.open("w") as f:
        json.dump(output, f, indent=2)
    size_kb = OUTPUT.stat().st_size / 1024
    print(f"\nWrote {OUTPUT} ({size_kb:.1f} KB, {len(output['entry'])} entries)")


if __name__ == "__main__":
    main()
