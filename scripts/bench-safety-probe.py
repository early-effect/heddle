#!/usr/bin/env python3
"""Matched-config safety probe. One server, already listening. Not a rate comparison.

Usage: bench-safety-probe.py PORT heddle|zio-http
"""

import socket
import sys
import time

HEADER = 16 * 1024
BODY = 10 * 1024 * 1024
READ_S = 2.0
HOLD = 32

PROFILE = (
    f"headerBytes={HEADER} bodyBytes={BODY} readTimeout={int(READ_S * 1000)}ms "
    "backlog=100 tcpNoDelay=true host=127.0.0.1"
)


def status_of(data: bytes) -> str:
    if not data:
        return "EMPTY"
    line = data.split(b"\r\n", 1)[0]
    parts = line.split()
    if len(parts) >= 2 and parts[0].startswith(b"HTTP/"):
        return parts[1].decode("ascii", "replace")
    return "NONE"


def connect(port: int, timeout: float) -> socket.socket:
    sock = socket.create_connection(("127.0.0.1", port), timeout=timeout)
    sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
    sock.settimeout(timeout)
    return sock


def read_headers(sock: socket.socket, limit: int = 65536) -> bytes:
    data = b""
    while b"\r\n\r\n" not in data and len(data) < limit:
        part = sock.recv(4096)
        if not part:
            break
        data += part
    return data


def exchange(port: int, payload: bytes, timeout: float, shutdown: bool = False):
    t0 = time.perf_counter()
    try:
        sock = connect(port, timeout)
    except OSError:
        return "RESET", (time.perf_counter() - t0) * 1000
    try:
        if payload:
            sock.sendall(payload)
        if shutdown:
            sock.shutdown(socket.SHUT_WR)
        data = read_headers(sock)
        if data:
            return status_of(data), (time.perf_counter() - t0) * 1000
        return "CLOSED", (time.perf_counter() - t0) * 1000
    except socket.timeout:
        return "HELD", (time.perf_counter() - t0) * 1000
    except OSError:
        return "RESET", (time.perf_counter() - t0) * 1000
    finally:
        sock.close()


def padded(n: int) -> bytes:
    prefix = b"GET /plaintext HTTP/1.1\r\nHost: localhost\r\nX-Pad: "
    suffix = b"\r\n\r\n"
    fill = n - len(prefix) - len(suffix)
    if fill < 1:
        raise SystemExit(f"pad {n} is smaller than the request skeleton")
    return prefix + (b"a" * fill) + suffix


def dribble(port: int, window: float = 3.2, gap: float = 0.4):
    partial = b"GET /plaintext HTTP/1.1\r\nHost: localhost\r\nX-Slow: "
    t0 = time.perf_counter()
    try:
        sock = connect(port, window + 1)
    except OSError:
        return "RESET", (time.perf_counter() - t0) * 1000
    data = b""
    try:
        sock.sendall(partial)
        sock.settimeout(gap)
        while time.perf_counter() - t0 < window and not data:
            time.sleep(gap)
            try:
                sock.send(b"a")
            except OSError:
                sock.settimeout(0.5)
                try:
                    part = sock.recv(4096)
                    if part:
                        data += part
                except OSError:
                    pass
                break
            try:
                part = sock.recv(4096)
                if part:
                    data += part
            except socket.timeout:
                pass
            except OSError:
                break
    finally:
        sock.close()
    elapsed = (time.perf_counter() - t0) * 1000
    if data:
        return status_of(data), elapsed
    return "HELD", elapsed


def oversized_body(port: int):
    cl = 11 * 1024 * 1024
    t0 = time.perf_counter()
    try:
        sock = connect(port, 3)
    except OSError:
        return "RESET", 0, (time.perf_counter() - t0) * 1000
    sent = 0
    data = b""
    try:
        head = f"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: {cl}\r\n\r\n".encode()
        sock.sendall(head)
        chunk = b"x" * (64 * 1024)
        while sent < cl and not data:
            try:
                n = sock.send(chunk)
            except OSError:
                break
            if n <= 0:
                break
            sent += n
            sock.settimeout(0.05)
            try:
                part = sock.recv(4096)
                if part:
                    data += part
            except socket.timeout:
                pass
            except OSError:
                break
        if not data:
            sock.settimeout(0.5)
            try:
                data = sock.recv(4096)
            except OSError:
                data = b""
    finally:
        sock.close()
    return status_of(data) if data else "CLOSED", sent, (time.perf_counter() - t0) * 1000


def over_cap(port: int):
    """Open HOLD + 1 keep-alive GETs. A refusal after at least HOLD - 1 successes is the cap.
    The readiness curl can still occupy one slot, so 31 then reset is the same win as 32 then reset.
    """
    socks = []
    t0 = time.perf_counter()
    request = b"GET /plaintext HTTP/1.1\r\nHost: localhost\r\n\r\n"
    refused = None
    try:
        for _ in range(HOLD + 1):
            try:
                sock = connect(port, 2)
                sock.sendall(request)
                data = read_headers(sock)
            except OSError:
                refused = "RESET"
                break
            status = status_of(data)
            if status != "200":
                refused = status or "EMPTY"
                sock.close()
                break
            socks.append(sock)
        elapsed = (time.perf_counter() - t0) * 1000
        held = len(socks)
        if refused is None:
            return "200", elapsed
        if held < HOLD - 1:
            return f"hold-{refused}-{held}", elapsed
        return f"{refused}-after-{held}", elapsed
    finally:
        for sock in socks:
            try:
                sock.close()
            except OSError:
                pass


def main() -> int:
    if len(sys.argv) != 3 or sys.argv[2] not in ("heddle", "zio-http"):
        print("usage: bench-safety-probe.py PORT heddle|zio-http", file=sys.stderr)
        return 2
    port = int(sys.argv[1])
    name = sys.argv[2]
    heddle = name == "heddle"
    cap_expect = "fail" if heddle else "200"
    print(f"safety probe  {name}  :{port}")
    print(f"profile {PROFILE}")
    if heddle:
        print(f"maxConnections={HOLD} on heddle. zio-http has no connection cap.")
    else:
        print("maxConnections=unset on zio-http. The heddle probe sets 32.")
    print(f"{'case':<22} {'result':<16} {'ms':>8}  expected effect")

    rows = []

    def add(case: str, result: str, ms: float, expect, effect: str):
        rows.append((case, result, ms, expect))
        print(f"{case:<22} {result:<16} {ms:8.0f}  {effect}")

    add("hello-open", *exchange(port, b"HELLO\r\n\r\n", 1.0), "400",
        "both 400. A finished non-preface line is not held for 24 bytes.")
    add("silent", *exchange(port, b"GET /plaintext HTTP/1.1\r\nHost: localhost\r\nX-Slow: ", 3.5), "408" if heddle else "not-held",
        "both release at 2s. Heddle writes 408. zio-http closes with no status.")
    add("dribble", *dribble(port), "408" if heddle else "HELD",
        "heddle 408. The budget does not reset per byte. zio-http stays open.")
    add("pad-6k", *exchange(port, padded(6 * 1024), 2), "200",
        "both 200. 6KiB is under the shared 16KiB header cap.")
    add("pad-12k", *exchange(port, padded(12 * 1024), 2), "200",
        "both 200. 12KiB is under the shared 16KiB header cap.")
    add("pad-20k", *exchange(port, padded(20 * 1024), 2), "431",
        "both 431. 20KiB is over the shared 16KiB header cap.")
    body = b"x" * (150 * 1024)
    post = f"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: {len(body)}\r\n\r\n".encode() + body
    add("body-150k", *exchange(port, post, 3), "200",
        "both 200. 150KiB is under the shared 10MiB body cap. Not a gap.")
    add(
        "cl-11m-nobody",
        *exchange(port, f"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: {11 * 1024 * 1024}\r\n\r\n".encode(), 2),
        "413",
        "both 413 from the declared length, before the body is read.",
    )
    status, sent, ms = oversized_body(port)
    add("cl-11m-written", status, ms, "413",
        "both 413, and neither reads the whole body.")
    print(f"{'cl-11m-bytes':<22} {sent:<16} {'':>8}  bytes queued before the 413. One socket buffer, not a full read.")
    add(
        "cl-te",
        *exchange(
            port,
            b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 4\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n",
            2,
        ),
        "400",
        "both 400. Content-Length and Transfer-Encoding conflict.",
    )
    add(
        "cl-plus",
        *exchange(port, b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: +5\r\n\r\nhello", 2),
        "400",
        "both 400. Content-Length is not 1*DIGIT.",
    )
    add("hello-shutdown", *exchange(port, b"HELLO\r\n\r\n", 1.0, shutdown=True), "400",
        "both 400. Half-close does not change a bad request line.")
    add(
        "two-cl",
        *exchange(
            port,
            b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 4\r\nContent-Length: 4\r\n\r\nping",
            2,
        ),
        "400" if heddle else None,
        "heddle 400. Repeated Content-Length is illegal. zio-http is recorded.",
    )
    add("over-cap", *over_cap(port), cap_expect,
        "heddle refuses the extra connection. zio-http accepts it. zio-http has no cap.")

    failed = []
    for case, result, _ms, expect in rows:
        if expect is None:
            continue
        if expect == "not-held" and result == "HELD":
            failed.append(case)
        elif expect == "fail" and (result in ("200", "HELD") or result.startswith("hold-")):
            failed.append(case)
        elif expect == "HELD" and result != "HELD":
            failed.append(case)
        elif expect not in ("not-held", "fail", "HELD") and result != expect:
            failed.append(case)
    if sent > (11 * 1024 * 1024) // 2:
        failed.append("cl-11m-bytes")
        print(f"cl-11m-bytes wrote {sent} bytes before the response. That is still reading the body.")
    if failed:
        print("FAIL " + " ".join(failed))
        return 1
    print("ok")
    return 0


if __name__ == "__main__":
    sys.exit(main())
