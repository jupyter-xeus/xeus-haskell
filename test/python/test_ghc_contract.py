"""Tier-2 fixtures for the browser-only GHC kernel protocol."""

import json
from pathlib import Path

from common.contract import display_value, error_text, visible_text
from fixtures import ghc_replies


REPO_ROOT = Path(__file__).resolve().parents[2]


def test_execution_persistence_and_mixed_cell_fixtures() -> None:
    assert ghc_replies.SUCCESS["content"]["status"] == "ok"
    assert "42" in visible_text(ghc_replies.EXPRESSION)
    assert "42" in visible_text(ghc_replies.MIXED_CELL)
    assert "hello" in visible_text(ghc_replies.STDOUT)


def test_error_fixture_uses_jupyter_error_shape() -> None:
    assert ghc_replies.ERROR["content"]["status"] == "error"
    assert "missingName" in error_text(ghc_replies.FAILURE)


def test_completion_inspection_and_completeness_fixtures() -> None:
    completion = ghc_replies.COMPLETION["content"]
    assert completion["status"] == "ok"
    assert "sharedAnswer" in completion["matches"]
    assert (completion["cursor_start"], completion["cursor_end"]) == (0, 9)

    inspection = ghc_replies.INSPECTION["content"]
    assert inspection["status"] == "ok"
    assert inspection["found"] is True
    assert "sharedAnswer ::" in inspection["data"]["text/plain"]
    assert ghc_replies.COMPLETENESS["content"]["status"] == "incomplete"


def test_html_latex_markdown_and_silent_fixtures() -> None:
    assert display_value(ghc_replies.RICH_DISPLAYS, "text/html") == "<b>HTML</b>"
    assert display_value(ghc_replies.RICH_DISPLAYS, "text/latex") == "$x^2$"
    assert display_value(
        ghc_replies.RICH_DISPLAYS, "text/markdown"
    ) == "**Markdown**"
    assert visible_text([]) == ""
    assert display_value([], "text/html") is None


def test_both_kernelspec_templates_share_the_jupyter_contract() -> None:
    templates = {
        "microhs": REPO_ROOT
        / "xhaskell/microhs/share/jupyter/kernels/xhaskell-mhs/kernel.json.in",
        "ghc": REPO_ROOT
        / "xhaskell/ghc/share/jupyter/kernels/xhaskell-ghc/kernel.json.in",
    }
    for kernel, template in templates.items():
        text = template.read_text(encoding="utf-8")
        assert '"language": "haskell"' in text, kernel
        assert '"kernel_protocol_version": "5.6.0"' in text, kernel
        assert "{connection_file}" in text, kernel
        assert f"xhaskell-{'mhs' if kernel == 'microhs' else 'ghc'}" in text

    ghc_template = templates["ghc"].read_text(encoding="utf-8")
    assert "Haskell (GHC @XEUS_HASKELL_GHC_VERSION@)" in ghc_template
    assert set(json.loads(ghc_template)["metadata"]["shared"]) == {
        "browser_wasi_shim.mjs",
        "browser_wasi_shim.mjs.map",
        "bsdtar.wasm",
        "dyld.mjs",
        "post-link.mjs",
        "prelude.mjs",
        "rootfs.tar.zst",
    }
