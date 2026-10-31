#!/usr/bin/env python3
"""Exercise LSP transport, malformed messages, and workspace requests on real disk files."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
import time


class Server:
    def __init__(self, binary, folder, exclusions=(), versioned=True):
        self.process = subprocess.Popen([binary], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        self.sequence = 0
        self.notifications = []
        self.request("initialize", {
            "workspaceFolders": [{"uri": folder.as_uri(), "name": "test"}],
            "capabilities": {"workspace": {"workspaceEdit": {"documentChanges": versioned}}},
            "initializationOptions": {"excludePaths": list(exclusions)},
        })

    def send(self, method, params, request_id=None):
        message = {"jsonrpc": "2.0", "method": method, "params": params}
        if request_id is not None:
            message["id"] = request_id
        data = json.dumps(message).encode()
        self.process.stdin.write(f"Content-Length: {len(data)}\r\n\r\n".encode() + data)
        self.process.stdin.flush()

    def request(self, method, params):
        self.sequence += 1
        self.send(method, params, self.sequence)
        while True:
            header = self.process.stdout.readline()
            assert header, f"server exited: {self.process.poll()}"
            length = int(header.split(b":", 1)[1])
            assert self.process.stdout.readline() == b"\r\n"
            response = json.loads(self.process.stdout.read(length))
            if response.get("id") == self.sequence:
                return response
            self.notifications.append(response)

    def query(self, method, path, line, column, **extra):
        return self.request("textDocument/" + method, {
            "textDocument": {"uri": path.as_uri()},
            "position": {"line": line, "character": column}, **extra,
        })

    def open(self, path, text, version=1):
        self.send("textDocument/didOpen", {"textDocument": {
            "uri": path.as_uri(), "languageId": "casa", "text": text, "version": version,
        }})

    def close(self):
        self.process.stdin.close()
        assert self.process.wait(timeout=10) == 0


def check_malformed_messages(binary):
    with tempfile.TemporaryDirectory(prefix="casa-malformed-") as temporary:
        document = Path(temporary) / "document.casa"
        uri = {"uri": document.as_uri()}
        server = Server(binary, Path(temporary))
        try:
            server.process.stdin.write(b"Content-Length: 5\r\n\r\n{oops")
            server.send("textDocument/didOpen", {"textDocument": uri})
            server.send("textDocument/didChange", {"textDocument": uri, "contentChanges": []})
            server.send("textDocument/didSave", {})
            server.send("textDocument/didClose", None)
            for method, params in (
                ("definition", {"position": {"line": 0, "character": 0}}),
                ("hover", {"textDocument": uri}),
                ("completion", {"textDocument": uri, "position": {"line": "0", "character": 0}}),
                ("references", None),
                ("rename", {"textDocument": uri, "position": {"line": 0, "character": 0}}),
                ("semanticTokens/full", [uri]),
            ):
                response = server.request("textDocument/" + method, params)
                assert response.get("error", {}).get("code") == -32602, (method, response)
            assert any(item.get("id", 0) is None and item["error"]["code"] == -32700
                       for item in server.notifications), server.notifications
            server.open(document, "fn greet { }\ngreet\n")
            assert server.query("definition", document, 1, 0)["result"] is not None
            for method in ("definition", "hover"):
                assert server.query(method, document, 1, -1)["result"] is None, method
            assert server.query("completion", document, -2, 0)["result"]["items"] == []
            assert server.request("shutdown", None)["result"] is None
        finally:
            server.close()


def check_query_reuse(binary):
    """Queries reuse a current analysis and analyze again after an import changes."""
    with tempfile.TemporaryDirectory(prefix="casa-query-reuse-") as temporary:
        root = Path(temporary)
        dependency = root / "dependency.casa"
        document = root / "document.casa"
        dependency.write_text("pub fn value -> i64 { 42 }\n")
        document.write_text('import "dependency.casa" as dependency\ndependency::value drop\n')
        server = Server(binary, root)

        def hover():
            published = len(server.notifications)
            contents = server.query("hover", document, 1, 13)["result"]["contents"]["value"]
            return contents, any(item.get("method") == "textDocument/publishDiagnostics"
                                 and item["params"]["uri"] == document.as_uri()
                                 for item in server.notifications[published:])

        try:
            server.open(document, document.read_text())
            assert "i64" in hover()[0]
            contents, analyzed = hover()
            assert "i64" in contents and not analyzed, (contents, analyzed)
            dependency.write_text('pub fn value -> str { "text" }\n')
            contents, analyzed = hover()
            assert "str" in contents and analyzed, (contents, analyzed)
            server.open(dependency, "pub fn value -> bool { true }\n")
            contents, analyzed = hover()
            assert "bool" in contents and analyzed, (contents, analyzed)
        finally:
            server.close()


def check_imported_diagnostics(binary):
    """Imported files publish their diagnostics until no open document imports them."""
    with tempfile.TemporaryDirectory(prefix="casa-imported-diagnostics-") as temporary:
        root = Path(temporary)
        dependency = root / "dependency.casa"
        first = root / "first.casa"
        second = root / "second.casa"
        broken = 'pub fn value -> i64 { "text" }\n'
        fixed = "pub fn value -> i64 { 42 }\n"
        importer = 'import "dependency.casa" as dependency\ndependency::value drop\n'
        dependency.write_text(broken)
        server = Server(binary, root)

        def published(since):
            """Each diagnostics list published for the dependency since a notification count."""
            return [item["params"]["diagnostics"] for item in server.notifications[since:]
                    if item.get("method") == "textDocument/publishDiagnostics"
                    and item["params"]["uri"] == dependency.as_uri()]

        def sync():
            """Collect the notifications sent before a request's response."""
            assert server.query("hover", root / "unopened.casa", 0, 0)["result"] is None

        def analyze_first():
            """Query first after its inputs changed, so it analyzes again."""
            count = len(server.notifications)
            server.query("hover", first, 1, 13)
            assert any(item.get("method") == "textDocument/publishDiagnostics"
                       and item["params"]["uri"] == first.as_uri()
                       for item in server.notifications[count:]), "first analyzed again"

        try:
            server.open(first, importer)
            sync()
            lists = published(0)
            assert len(lists) == 1 and len(lists[0]) == 1, lists
            assert lists[0][0]["range"]["start"]["line"] == 0, lists

            # Two importers report the same diagnostic once.
            count = len(server.notifications)
            server.open(second, importer)
            sync()
            assert published(count) == [lists[0]], published(count)

            # Fixing the file clears its diagnostics on the next analysis.
            dependency.write_text(fixed)
            count = len(server.notifications)
            server.send("textDocument/didSave", {"textDocument": {"uri": first.as_uri()}})
            server.send("textDocument/didSave", {"textDocument": {"uri": second.as_uri()}})
            sync()
            assert published(count)[-1] == [], published(count)

            # Closing the last importer clears its imported files.
            dependency.write_text(broken)
            server.send("textDocument/didSave", {"textDocument": {"uri": second.as_uri()}})
            sync()
            assert len(published(0)[-1]) == 1, published(0)
            server.send("textDocument/didClose", {"textDocument": {"uri": second.as_uri()}})
            sync()
            assert len(published(0)[-1]) == 1, "first still imports the file"
            count = len(server.notifications)
            server.send("textDocument/didClose", {"textDocument": {"uri": first.as_uri()}})
            sync()
            assert published(count) == [[]], published(count)

            # An open file publishes only its own analysis, also after importers analyze it.
            server.open(first, importer)
            server.open(dependency, "pub fn value -> i64 { true }\n")
            sync()
            own = published(0)[-1]
            assert len(own) == 1, own
            count = len(server.notifications)
            analyze_first()
            server.send("textDocument/didChange", {
                "textDocument": {"uri": dependency.as_uri(), "version": 2},
                "contentChanges": [{"text": fixed}],
            })
            analyze_first()
            assert published(count) == [[]], published(count)

            # Closing it hands its diagnostics back to the importer, which reads the disk text.
            count = len(server.notifications)
            server.send("textDocument/didClose", {"textDocument": {"uri": dependency.as_uri()}})
            sync()
            assert published(count) and published(count)[-1] == lists[0], published(count)
            assert server.request("shutdown", None)["result"] is None
        finally:
            server.close()


def main(binary):
    with tempfile.TemporaryDirectory(prefix="casa workspace ") as temporary:
        root = Path(temporary)
        shared = root / "shared.casa"
        first = root / "first.casa"
        second = root / "second.casa"
        unrelated = root / "unrelated.casa"
        shared.write_text("pub fn greet { }\ngreet\n")
        first.write_text('import "shared.casa" as shared\nshared::greet\n')
        second.write_text(first.read_text())
        unrelated.write_text("fn greet { }\ngreet\n")
        (root / "cycle").symlink_to(root, target_is_directory=True)
        server = Server(binary, root)
        try:
            server.open(shared, shared.read_text(), 7)
            response = server.query("references", shared, 0, 7, context={"includeDeclaration": True})
            assert len(response["result"]) == 4, response
            assert unrelated.as_uri() not in {item["uri"] for item in response["result"]}
            response = server.query("rename", shared, 0, 7, newName="hello")
            assert "error" not in response, response
            changes = response["result"]["documentChanges"]
            assert len(changes) == 3, changes
            assert sum(len(change["edits"]) for change in changes) == 4
            versions = {change["textDocument"]["uri"]: change["textDocument"]["version"] for change in changes}
            assert versions == {shared.as_uri(): 7, first.as_uri(): None, second.as_uri(): None}, versions
            with tempfile.TemporaryDirectory(prefix="casa-external-") as external:
                target = Path(external) / "importer.casa"
                target.write_text(f'import "{shared}" as shared\nshared::greet\n')
                link = root / "linked.casa"
                link.symlink_to(target)
                assert len(server.query("references", shared, 0, 7)["result"]) == 5
                linked_rename = server.query("rename", shared, 0, 7, newName="hi")
                assert "error" not in linked_rename, linked_rename
                assert link.as_uri() in {item["textDocument"]["uri"] for item in linked_rename["result"]["documentChanges"]}
                target.chmod(0o444)
                assert "error" in server.query("rename", shared, 0, 7, newName="hi")
                target.chmod(0o644)
                target.unlink()
                assert "error" in server.query("rename", shared, 0, 7, newName="hi")
                link.unlink()
            for name in ("if", "one two", "one::two", "greet\nother", "&greet"):
                assert "error" in server.query("rename", shared, 0, 7, newName=name), name
            shared.write_text("pub fn wrong { }\n")
            server.send("textDocument/didSave", {"textDocument": {"uri": shared.as_uri()}})
            assert len(server.query("references", first, 1, 8)["result"]) == 4
            damaged = root / "damaged.casa"
            damaged.write_text('import "missing.casa"\n')
            assert "error" in server.query("rename", shared, 0, 7, newName="hello")
            damaged.unlink()
            # Unsaved importers participate in discovery.
            unsaved = root / "unsaved.casa"
            server.open(unsaved, first.read_text(), 3)
            assert len(server.query("references", shared, 0, 7)["result"]) == 5
            server.send("textDocument/didClose", {"textDocument": {"uri": unsaved.as_uri()}})
            assert len(server.query("references", shared, 0, 7)["result"]) == 4
            # Candidate reanalysis rejects a declaration collision.
            collision = "pub fn greet { }\nfn hello { }\ngreet\n"
            server.open(shared, collision, 8)
            assert "error" in server.query("rename", shared, 0, 7, newName="hello")
            local = root / "local.casa"
            server.open(local, 'fn run { "😀" drop 1 = count count drop }\n', 1)
            response = server.query("rename", local, 0, 23, newName="total")
            assert "error" not in response, response
            edits = response["result"]["documentChanges"][0]["edits"]
            assert [edit["range"]["start"]["character"] for edit in edits] == [23, 29], edits
            server.open(local, "42 = count\ncount print\ncount drop", 2)
            response = server.query("rename", local, 1, 0, newName="total")
            assert "error" not in response, response
            edits = response["result"]["documentChanges"][0]["edits"]
            assert len(edits) == 3 and edits[0]["range"] == {
                "start": {"line": 0, "character": 5}, "end": {"line": 0, "character": 10},
            }, edits
            server.open(local, "fn first { 1 = value value drop }\nfn second { 2 = value value drop }", 3)
            response = server.query("rename", local, 0, 15, newName="renamed")
            assert "error" not in response, response
            edits = response["result"]["documentChanges"][0]["edits"]
            assert len(edits) == 2 and all(edit["range"]["start"]["line"] == 0 for edit in edits), edits
            server.open(local, "struct Point { x:i64 } fn take value:Point { value drop }", 4)
            assert "error" in server.query("rename", local, 0, 7, newName="Next")
            for version, newline in enumerate(("\r", "\r\n"), 5):
                server.open(local, newline.join(("42 = count", "count print", "count drop")), version)
                assert len(server.query("references", local, 1, 0)["result"]) == 3
                response = server.query("rename", local, 0, 5, newName="x")
                assert "error" not in response, response
                edits = response["result"]["documentChanges"][0]["edits"]
                assert [edit["range"]["start"] for edit in edits] == [
                    {"line": 0, "character": 5}, {"line": 1, "character": 0}, {"line": 2, "character": 0},
                ], edits
            assert server.query("references", shared, -1, 0)["result"] is None
            assert server.query("references", shared, 0, -1)["result"] is None
            # A disk edit is observed even without a file notification.
            second.write_text('import "shared.casa" as shared\nshared::greet shared::greet\n')
            assert len(server.query("references", shared, 0, 7)["result"]) == 5
            second.write_text(first.read_text())
            # Newer document versions win and a stale change cannot replace them.
            server.send("textDocument/didChange", {"textDocument": {"uri": shared.as_uri(), "version": 9},
                        "contentChanges": [{"text": "pub fn greet { }\ngreet greet\n"}]})
            server.send("textDocument/didChange", {"textDocument": {"uri": shared.as_uri(), "version": 8},
                        "contentChanges": [{"text": "pub fn obsolete { }"}]})
            assert len(server.query("references", shared, 0, 7)["result"]) == 5
            # Queue an edit behind rename. A synchronous request must not publish stale edits.
            server.sequence += 1
            server.send("textDocument/rename", {"textDocument": {"uri": shared.as_uri()},
                        "position": {"line": 0, "character": 7}, "newName": "hello"}, server.sequence)
            server.send("textDocument/didChange", {"textDocument": {"uri": shared.as_uri(), "version": 10},
                        "contentChanges": [{"text": "pub fn greet { }\ngreet\n"}]})
            prior = server.sequence
            server.query("references", shared, 0, 7)
            assert any(item.get("id") == prior and "error" in item for item in server.notifications)

        finally:
            server.close()
        server = Server(binary, root, exclusions=[str(second)])
        try:
            shared.write_text("pub fn greet { }\ngreet\n")
            assert len(server.query("references", shared, 0, 7)["result"]) == 3
        finally:
            server.close()
        server = Server(binary, root, versioned=False)
        try:
            assert "error" in server.query("rename", shared, 0, 7, newName="hello")
        finally:
            server.close()
        server = Server(binary, root)
        try:
            server.send("textDocument/didOpen", {"textDocument": {
                "uri": shared.as_uri(), "languageId": "casa", "text": shared.read_text(),
            }})
            assert "error" in server.query("rename", shared, 0, 7, newName="hello")
        finally:
            server.close()
    with tempfile.TemporaryDirectory(prefix="casa-workspace-revision-") as temporary:
        root = Path(temporary)
        shared = root / "shared.casa"
        original = "pub fn greet { }\ngreet\n"
        shared.write_text(original)
        (root / "slow.casa").write_text('import "shared.casa" as s\ns::greet\n' +
                                        "\n".join(f"fn worker{i} {{ }}" for i in range(2500)))
        server = Server(binary, root)
        written = []

        def replace_source():
            time.sleep(0.03)
            shared.write_text("# moved\n" + original)
            written.append(time.monotonic())

        writer = threading.Thread(target=replace_source)
        try:
            writer.start()
            response = server.query("references", shared, 0, 7)
            received = time.monotonic()
            writer.join()
            if written[0] < received:
                assert response.get("result") is None, response
        finally:
            writer.join()
            server.close()
    check_malformed_messages(binary)
    check_query_reuse(binary)
    check_imported_diagnostics(binary)
    print("LSP transport and workspace tests passed")


if __name__ == "__main__":
    main(sys.argv[1])
