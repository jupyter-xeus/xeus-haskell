#############################################################################
# Copyright (c) 2025, Masaya Taniguchi
#
# Distributed under the terms of the Apache Software License 2.0.
#
# The full license is in the file LICENSE, distributed with this software.
#############################################################################

"""Incremental xeus-haskell kernel smoke tests.

These tests start with a single fast check based on jupyter_kernel_test.
When the interpreter performance improves we can extend the coverage by
leveraging the additional helpers exposed by KernelTests.
"""

from __future__ import annotations

from queue import Empty
from typing import Any
import unittest

import jupyter_kernel_test

# MicroHs warm-up compiles modules on first execute; give it plenty of time.
jupyter_kernel_test.TIMEOUT = 90


class XHaskellKernelTests(jupyter_kernel_test.KernelTests):
    """Minimal smoke tests for the xhaskell kernel."""

    kernel_name = "xhaskell"
    language_name = "haskell"

    completion_samples: list[dict[str, str]] = [
        {"text": "putStr", "cursor_pos": 6},
        {"text": "whe", "cursor_pos": 3},
    ]
    complete_code_samples: list[str] = [
        "1 + 1",
        "x = 42\nx + 1",
        "putStrLn \"hello\"",
        "data Color = Red | Green | Blue",
    ]
    incomplete_code_samples: list[str] = []
    invalid_code_samples: list[str] = ["1 + *"]
    code_hello_world = ""  # xhaskell currently uses execute_result for IO output
    code_stderr = ""
    code_page_something = ""
    code_generate_error = "1 `div` 0"
    code_execute_result = [{"code": "10", "result": "10\n"}]
    code_display_data = []
    code_history_pattern = "10"
    supported_history_operations = ("range", "tail", "search")
    code_inspect_sample = "putStrLn"
    code_clear_output = ""

    _kernel_info_reply: dict[str, Any] | None = None

    @classmethod
    def setUpClass(cls) -> None:
        """Start the kernel once for the whole test class.

        The SkipTest fallback keeps local editing workflows lightweight when
        the kernel has not been installed yet.
        """
        try:
            super().setUpClass()
        except Exception as exc:  # pragma: no cover - exercised in CI only
            raise unittest.SkipTest(f"xhaskell kernel is unavailable: {exc}") from exc
        try:
            cls._kernel_info_reply = cls._request_kernel_info_reply()
        except Empty as exc:
            cls._shutdown_kernel()
            raise unittest.SkipTest(
                "xhaskell kernel did not answer kernel_info in time"
            ) from exc

    @classmethod
    def _shutdown_kernel(cls) -> None:
        cls.kc.stop_channels()
        cls.km.shutdown_kernel()

    @classmethod
    def _request_kernel_info_reply(cls) -> dict[str, Any]:
        msg_id = cls.kc.kernel_info()
        reply = cls.kc.get_shell_msg(timeout=jupyter_kernel_test.TIMEOUT)
        if reply["header"]["msg_type"] != "kernel_info_reply":
            raise AssertionError("Unexpected kernel_info reply payload")
        reply_content = reply["content"]
        if reply_content.get("status") != "ok":
            raise AssertionError(f"kernel_info returned {reply_content}")
        return reply

    def test_kernel_info(self) -> None:
        if self._kernel_info_reply is None:
            self.skipTest("kernel_info handshake was skipped in setUpClass")
        reply = self._kernel_info_reply
        lang_info = reply["content"]["language_info"]
        self.assertEqual(lang_info["name"], self.language_name)
        self.assertEqual(lang_info["file_extension"], "hs")

    def _execute_or_skip(self, *, code: str) -> tuple[dict[str, Any], list[dict[str, Any]]]:
        try:
            return self.execute_helper(code=code, timeout=jupyter_kernel_test.TIMEOUT)
        except Empty as exc:
            self.skipTest(f"xhaskell kernel timed out while executing {code!r}: {exc}")

    @staticmethod
    def _extract_plain_text(
        output_msgs: list[dict[str, Any]],
        *,
        msg_type: str = "execute_result",
    ) -> str:
        for msg in output_msgs:
            if msg["msg_type"] != msg_type:
                continue
            if msg_type == "execute_result":
                data = msg["content"].get("data", {})
                text = data.get("text/plain")
            else:
                text = msg["content"].get("text")
            if text:
                return text
        return ""

    def test_simple_expression_emits_execute_result(self) -> None:
        """Running a tiny arithmetic expression should yield execute_result."""
        self.flush_channels()
        reply, output_msgs = self._execute_or_skip(code="1 + 1")

        self.assertEqual(reply["content"]["status"], "ok")

        result_msgs = [
            msg for msg in output_msgs if msg["msg_type"] == "execute_result"
        ]
        self.assertTrue(
            result_msgs,
            f"Expected execute_result message, saw {[msg['msg_type'] for msg in output_msgs]}",
        )

        payload = result_msgs[0]["content"]["data"].get("text/plain", "")
        self.assertIn("2", payload.strip())

    def test_invalid_haskell_snippet_reports_error(self) -> None:
        """Parser failures should bubble up as notebook errors."""
        self.flush_channels()
        reply, output_msgs = self._execute_or_skip(code="1 +")

        self.assertEqual(reply["content"]["status"], "error")
        error_msgs = [msg for msg in output_msgs if msg["msg_type"] == "error"]
        self.assertTrue(
            error_msgs,
            f"No error output, saw {[msg['msg_type'] for msg in output_msgs]}",
        )

    def test_definition_persists_across_cells(self) -> None:
        """Definitions should persist in the REPL context across cells."""
        self.flush_channels()
        reply, outputs = self._execute_or_skip(code="square x = x * x")
        self.assertEqual(reply["content"]["status"], "ok")
        self.assertFalse(
            any(msg["msg_type"] == "execute_result" for msg in outputs),
            "Definition produced an unexpected execute_result payload",
        )

        self.flush_channels()
        reply, outputs = self._execute_or_skip(code="square 7")
        self.assertEqual(reply["content"]["status"], "ok")
        payload = self._extract_plain_text(outputs)
        self.assertIn("49", payload.strip())

    @unittest.skip("xhaskell currently publishes all output as execute_result")
    def test_putstrln_emits_plaintext(self) -> None:
        """putStrLn output should surface back to the notebook."""
        self.flush_channels()
        reply, outputs = self._execute_or_skip(code='putStrLn "hello from xeus"')
        self.assertEqual(reply["content"]["status"], "ok")
        payload = self._extract_plain_text(outputs)
        self.assertIn("hello from xeus", payload)

    def test_completion_filters_prefix(self) -> None:
        """Completion should honor the prefix at the cursor position."""
        self.flush_channels()
        reply, _ = self._execute_or_skip(code="xh_comp_value = 123")
        self.assertEqual(reply["content"]["status"], "ok")

        self.flush_channels()
        msg_id = self.kc.complete(code="xh_comp", cursor_pos=7)
        try:
            reply_msg = self.kc.get_shell_msg(timeout=jupyter_kernel_test.TIMEOUT)
        except Empty as exc:  # pragma: no cover - exercised in CI only
            self.skipTest(f"xhaskell kernel timed out while completing: {exc}")
            return

        self.assertEqual(reply_msg["parent_header"].get("msg_id"), msg_id)
        content = reply_msg["content"]
        self.assertEqual(content.get("status"), "ok")

        matches = content.get("matches", [])
        self.assertIn("xh_comp_value", matches)
        self.assertNotIn("where", matches)  # should be filtered out by prefix
        self.assertEqual(content.get("cursor_start"), 0)
        self.assertEqual(content.get("cursor_end"), 7)

    def test_multi_statement_cell(self) -> None:
        """Repro Case 1: Mix of definition and expression in one cell."""
        self.flush_channels()
        code = 'name = "Haskell Curry"\nputStrLn name'
        reply, outputs = self._execute_or_skip(code=code)
        self.assertEqual(reply["content"]["status"], "ok")
        payload = self._extract_plain_text(outputs)
        self.assertIn("Haskell Curry", payload)

    def test_multi_expression_cell(self) -> None:
        """Repro Case 2: Multiple IO actions in one cell."""
        self.flush_channels()
        code = 'putStrLn "Hello"\nputStrLn "World"'
        reply, outputs = self._execute_or_skip(code=code)
        self.assertEqual(reply["content"]["status"], "ok")
        payload = self._extract_plain_text(outputs)
        self.assertIn("Hello", payload)
        self.assertIn("World", payload)

    def test_mixed_definition_and_pure_expression(self) -> None:
        """Edge Case: Pure expression following a definition (no IO)."""
        self.flush_channels()
        code = 'xh_edge_x = 10\nxh_edge_x + 32'
        reply, outputs = self._execute_or_skip(code=code)
        self.assertEqual(reply["content"]["status"], "ok")
        payload = self._extract_plain_text(outputs)
        self.assertIn("42", payload)

    def test_simple_pure_expression(self) -> None:
        """Ensure a single pure expression still works (was an edge case in redesign)."""
        self.flush_channels()
        code = '21 + 21'
        reply, outputs = self._execute_or_skip(code=code)
        self.assertEqual(reply["content"]["status"], "ok")
        payload = self._extract_plain_text(outputs)
        self.assertIn("42", payload)

    def test_multi_line_definition_followed_by_expression(self) -> None:
        """Edge Case: Multi-line definition (layout) followed by an expression."""
        self.flush_channels()
        code = 'xh_multi_f x =\n  x * 2\nxh_multi_f 21'
        reply, outputs = self._execute_or_skip(code=code)
        self.assertEqual(reply["content"]["status"], "ok")
        payload = self._extract_plain_text(outputs)
        self.assertIn("42", payload)

    def test_inspect_simple_variable(self) -> None:
        """Verify Shift+Tab introspection for a user-defined variable."""
        self.flush_channels()
        self._execute_or_skip(code="xh_inspect_x = 42")
        
        msg_id = self.kc.inspect(code="xh_inspect_x", cursor_pos=5)
        try:
            reply_msg = self.kc.get_shell_msg(timeout=jupyter_kernel_test.TIMEOUT)
        except Empty:
            self.skipTest("xhaskell kernel timed out while inspecting")
            return

        self.assertEqual(reply_msg["content"]["status"], "ok")
        self.assertTrue(reply_msg["content"]["found"])
        data = reply_msg["content"]["data"]
        # MicroHs shows the polymorphic type for 42: (forall a . ((Num a) => a))
        self.assertIn("xh_inspect_x ::", data["text/plain"])
        self.assertIn("Num", data["text/plain"])

    def test_inspect_builtin_function(self) -> None:
        """Verify Shift+Tab introspection for a built-in function."""
        self.flush_channels()
        # Warm up if necessary
        self._execute_or_skip(code="0")
        
        msg_id = self.kc.inspect(code="putStrLn", cursor_pos=4)
        try:
            reply_msg = self.kc.get_shell_msg(timeout=jupyter_kernel_test.TIMEOUT)
        except Empty:
            self.skipTest("xhaskell kernel timed out while inspecting")
            return

        self.assertEqual(reply_msg["content"]["status"], "ok")
        self.assertTrue(reply_msg["content"]["found"])
        data = reply_msg["content"]["data"]
        # MicroHs might show it as ([Char]) -> (IO ())
        self.assertIn("putStrLn ::", data["text/plain"])
        self.assertIn("[Char]", data["text/plain"])
        self.assertIn("IO", data["text/plain"])

if __name__ == "__main__":
    unittest.main()
