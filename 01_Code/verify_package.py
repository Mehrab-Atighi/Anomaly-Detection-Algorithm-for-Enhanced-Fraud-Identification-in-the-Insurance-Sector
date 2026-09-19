#!/usr/bin/env python3
"""Verify an extracted release against its manifest (standard library only)."""
from pathlib import Path
import json,hashlib
ROOT=Path(__file__).resolve().parents[1]
def main():
    manifest=json.loads((ROOT/"package_manifest.json").read_text())
    errors=[]
    for item in manifest["files"]:
        p=ROOT/item["path"]
        if not p.is_file(): errors.append("Missing: "+item["path"]);continue
        if p.stat().st_size!=item["bytes"] or hashlib.sha256(p.read_bytes()).hexdigest()!=item["sha256"]:
            errors.append("Changed: "+item["path"])
    if errors: raise SystemExit("\n".join(errors))
    print(f"PASS: {len(manifest['files'])} distributed files match the release manifest.")
    print("Regenerated or edited files will correctly be reported as changed; retain the original ZIP for provenance.")
if __name__=="__main__":main()
