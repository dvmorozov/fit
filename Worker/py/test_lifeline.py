"""The sidecar learns its parent is gone from an event, not from polling."""
import socket
import threading

import lifeline


def _listener():
    srv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    srv.bind(("127.0.0.1", 0))
    srv.listen(1)
    return srv, srv.getsockname()[1]


def test_connect_reaches_the_parents_port():
    srv, port = _listener()
    try:
        sock = lifeline.connect(port)
        conn, _ = srv.accept()
        conn.close()
        sock.close()
    finally:
        srv.close()


def test_announce_ready_writes_the_ready_line():
    srv, port = _listener()
    try:
        sock = lifeline.connect(port)
        conn, _ = srv.accept()
        lifeline.announce_ready(sock)
        conn.settimeout(5)
        assert conn.recv(16) == lifeline.READY_LINE
        conn.close()
        sock.close()
    finally:
        srv.close()


def test_the_parent_ending_is_noticed_by_the_blocked_read():
    srv, port = _listener()
    ended = threading.Event()
    try:
        sock = lifeline.connect(port)
        conn, _ = srv.accept()
        watcher = threading.Thread(
            target=lifeline.wait_for_parent_to_end, args=(sock, ended.set),
            daemon=True)
        watcher.start()
        assert not ended.wait(0.2), "nothing ended while the parent is there"
        conn.close()                      # the parent goes
        assert ended.wait(5), "the blocked read noticed at once"
        sock.close()
    finally:
        srv.close()


def test_data_from_the_parent_is_not_mistaken_for_its_end():
    srv, port = _listener()
    ended = threading.Event()
    try:
        sock = lifeline.connect(port)
        conn, _ = srv.accept()
        watcher = threading.Thread(
            target=lifeline.wait_for_parent_to_end, args=(sock, ended.set),
            daemon=True)
        watcher.start()
        conn.sendall(b"anything\n")
        assert not ended.wait(0.2)
        conn.close()
        assert ended.wait(5)
        sock.close()
    finally:
        srv.close()


def test_a_broken_connection_counts_as_the_parent_ending():
    #  A parent killed mid-write can reset rather than close the connection;
    #  either way it is gone, and the sidecar must not outlive it.
    class Reset:
        def recv(self, _n):
            raise ConnectionResetError("reset by peer")

    ended = threading.Event()
    lifeline.wait_for_parent_to_end(Reset(), ended.set)
    assert ended.is_set()


def test_the_port_is_read_before_anything_slow_is_imported():
    #  A sidecar that dies importing numpy must close its connection - so it has
    #  to have one before that import, which means reading the flag straight
    #  from argv at the top of fit_backend, not from argparse in main.
    srv, port = _listener()
    try:
        sock = lifeline.connect_from_argv(["fit_backend.py", "--port", "1",
                                           "--lifeline-port", str(port)])
        assert sock is not None
        conn, _ = srv.accept()
        conn.close()
        sock.close()
    finally:
        srv.close()


def test_no_lifeline_flag_connects_to_nothing():
    assert lifeline.connect_from_argv(["fit_backend.py", "--port", "1"]) is None


def test_an_unreadable_lifeline_port_connects_to_nothing():
    #  A typo must not stop a hand-started sidecar from running.
    assert lifeline.connect_from_argv(["x", "--lifeline-port", "nope"]) is None
    assert lifeline.connect_from_argv(["x", "--lifeline-port"]) is None


def test_a_port_nothing_listens_on_connects_to_nothing():
    #  A stale or mistyped port: the sidecar runs, it just has no one to tell.
    srv, port = _listener()
    srv.close()
    assert lifeline.connect_from_argv(["x", "--lifeline-port", str(port)]) is None
