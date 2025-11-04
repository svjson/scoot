import time
import uuid

from scoot_core import OperationEnv, Cache, Connection
from scoot_core.openv import Operation


class RequestContext:
    def __init__(self, path: str):
        self.id = str(uuid.uuid4())[:8]
        self.path = path
        self.start_time = time.perf_counter()
        self.spans = []
        self.root_span = _SpanNode(name=path, parent=None)
        self.stack = [self.root_span]

    def span(self, name):
        """Context manager for timing sub-operations."""
        return _Span(self, name)

    def format_tree(self, node=None, indent=0):
        bull = " - "
        if node is None:
            bull = ""
            node = self.root_span
        pad = " " * (indent - 1) if indent > 1 else ""
        out = [f"{pad}{bull}{node.name:<22} {node.duration_ms:>8.1f}ms"]
        for child in node.children:
            out.extend(self.format_tree(child, indent + 3))
        return out

    @property
    def total_ms(self):
        return (time.perf_counter() - self.start_time) * 1000


class _SpanNode:
    def __init__(self, name, parent):
        self.name = name
        self.parent = parent
        self.children = []
        self.start = time.perf_counter()
        self.end = None

    def stop(self):
        self.end = time.perf_counter()

    @property
    def duration_ms(self):
        if self.end is None:
            return 0.0
        return (self.end - self.start) * 1000.0


class _Span(Operation):
    """Context manager that records a nested span in RequestContext."""

    def __init__(self, ctx: RequestContext, name: str):
        super().__init__(name)
        self.ctx = ctx
        self.node = None

    def __enter__(self):
        parent = self.ctx.stack[-1]
        self.node = _SpanNode(self.name, parent)
        parent.children.append(self.node)
        self.ctx.stack.append(self.node)
        return self

    def __exit__(self, _exc_type, _exc, _tb):
        if self.node is not None:
            self.node.stop()
            if self.ctx.stack and self.ctx.stack[-1] is self.node:
                self.ctx.stack.pop()


class ServerOperation(OperationEnv):

    def __init__(
        self,
        reqenv: RequestContext,
        connection: Connection,
        cache: dict[str, Cache],
    ):
        super().__init__(connection, cache)
        self.reqenv = reqenv

    def operation(self, name):
        return self.reqenv.span(name)

    def end_operations(self):
        self.reqenv.root_span.stop()

    def format_operation_metrics(self) -> list[str]:
        return self.reqenv.format_tree()
