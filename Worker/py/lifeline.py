"""A child's end of the readiness channel (Common/readiness_channel.pas).

The parent - fit_server - listens on a loopback port and passes it here. The
sidecar connects at once, writes "ready" when it is listening for its real work,
and then blocks reading the same connection: when the parent ends, cleanly or
killed, the operating system closes it and the read returns end-of-file. That is
how the sidecar never outlives its server - by waiting for an event, not by
asking every few seconds whether a process id still exists.
"""
import socket

READY_LINE = b"ready\n"


def connect(port: int, host: str = "127.0.0.1"):
    """Connects to the parent's readiness port and returns the socket."""
    return socket.create_connection((host, port))


def connect_from_argv(argv):
    """Connects to the port given as --lifeline-port in argv, or returns None.

    Read straight from argv, not through argparse in main, because it has to
    happen before fit_backend imports numpy, scipy and lmfit: a sidecar that dies
    on one of those imports must already have a connection to close, or its
    parent waits out the whole budget for a process that is gone."""
    try:
        port = int(argv[argv.index("--lifeline-port") + 1])
    except (ValueError, IndexError):
        return None
    if port <= 0:
        return None
    try:
        return connect(port)
    except OSError:
        return None


def announce_ready(sock) -> None:
    """Tells the parent the sidecar is listening."""
    sock.sendall(READY_LINE)


def wait_for_parent_to_end(sock, on_end) -> None:
    """Blocks until the parent's end of the connection closes, then calls on_end.

    Anything the parent sends is read and ignored: only the end of the
    connection means something here."""
    try:
        while sock.recv(4096):
            pass
    except OSError:
        #  Reset rather than closed: the parent is gone all the same.
        pass
    on_end()
