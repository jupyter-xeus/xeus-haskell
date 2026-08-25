"""Representative replies produced by the browser-only GHC kernel adapter."""

SUCCESS = {"content": {"status": "ok"}}
ERROR = {"content": {"status": "error"}}

EXPRESSION = [
    {"msg_type": "execute_result", "content": {"data": {"text/plain": "42"}}},
]
MIXED_CELL = [
    {"msg_type": "execute_result", "content": {"data": {"text/plain": "42"}}},
]
STDOUT = [
    {"msg_type": "stream", "content": {"name": "stdout", "text": "hello\n"}},
]
FAILURE = [
    {
        "msg_type": "error",
        "content": {
            "ename": "GHCError",
            "evalue": "Not in scope: missingName",
            "traceback": ["Not in scope: missingName"],
        },
    },
]
RICH_DISPLAYS = [
    {"msg_type": "display_data", "content": {"data": {"text/html": "<b>HTML</b>"}}},
    {"msg_type": "display_data", "content": {"data": {"text/latex": "$x^2$"}}},
    {
        "msg_type": "display_data",
        "content": {"data": {"text/markdown": "**Markdown**"}},
    },
]
COMPLETION = {
    "content": {
        "status": "ok",
        "matches": ["sharedAnswer"],
        "cursor_start": 0,
        "cursor_end": 9,
    },
}
INSPECTION = {
    "content": {
        "status": "ok",
        "found": True,
        "data": {"text/plain": "sharedAnswer :: Integer"},
    },
}
COMPLETENESS = {"content": {"status": "incomplete", "indent": "  "}}
