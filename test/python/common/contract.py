"""Kernel-neutral helpers for asserting Jupyter protocol messages."""

from __future__ import annotations

from typing import Any


Message = dict[str, Any]


def visible_text(messages: list[Message]) -> str:
    chunks: list[str] = []
    for message in messages:
        content = message.get("content", {})
        if message.get("msg_type") == "execute_result":
            chunks.append(content.get("data", {}).get("text/plain", ""))
        elif message.get("msg_type") == "stream" and content.get("name") == "stdout":
            chunks.append(content.get("text", ""))
    return "\n".join(chunk for chunk in chunks if chunk)


def display_value(messages: list[Message], mime_type: str) -> str | None:
    for message in messages:
        if message.get("msg_type") != "display_data":
            continue
        data = message.get("content", {}).get("data", {})
        if mime_type in data:
            return data[mime_type]
    return None


def error_text(messages: list[Message]) -> str:
    for message in messages:
        if message.get("msg_type") != "error":
            continue
        content = message.get("content", {})
        traceback = content.get("traceback", [])
        return "\n".join(traceback) or content.get("evalue", "")
    return ""
